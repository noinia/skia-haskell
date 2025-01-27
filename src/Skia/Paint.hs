{-# LANGUAGE TemplateHaskell #-}
module Skia.Paint
  ( PaintSpec(PaintSpec)
  , antiAlias, color, strokeCap, strokeJoin, strokeMiter, strokeWidth



  , SkPaint
  -- , createCSkPaint
  ) where

import           Control.Lens
import           Data.Colour
import           Data.Default.Class
import           Effectful
import           Foreign.ForeignPtr
import           Foreign.Ptr
import qualified Skia.Canvas.Raw as Raw

--------------------------------------------------------------------------------

data AntiAliassed = NoAntiAliassing | WithAntiAliassing
  deriving (Show,Read,Eq)

instance Default AntiAliassed where
  def = NoAntiAliassing

data Cap = Butt
         | RoundCap
         | SquareCap
         | LastCap -- ^ largest Cap value
  deriving (Show,Read,Eq)

instance Default Cap where
  def = Butt


data Join = Join  -- ^ extends to miter limit
          | Round  -- ^ adds circle
          | Bevel -- ^ connects outside edges
          | LastJoin -- ^ equivalent to the largest value for Join
          deriving (Show,Eq,Read,Ord)


instance Default Join where
  def = Join


data PaintStyle = StrokeOnly | FillOnly | StrokeAndFill
  deriving (Show,Read,Eq)

instance Default PaintStyle where
  def = StrokeAndFill

data PaintSpec = PaintSpec { _antiAlias   :: AntiAliassed
                           , _color       :: AlphaColour Double
                           , _strokeCap   :: Cap
                           , _strokeJoin  :: Join
                           , _strokeMiter :: Float
                           -- ^ the limit at which a sharp corner is drawn beveled.
                           -- Valid values are zero and greater. Has no effect if miter is less than zero.
                           , _strokeWidth :: Float
                           , _style       :: PaintStyle
                           }
               deriving (Show,Eq)

-- maybe move Stroke Miter into Join?

makeLenses ''PaintSpec

instance Default PaintSpec where
  def = PaintSpec { _antiAlias   = def
                  , _color       = opaque black
                  , _strokeCap   = def
                  , _strokeJoin  = def
                  , _strokeMiter = 0 -- ?
                  , _strokeWidth = 1
                  , _style       = def
                  }

-- -- | Creates the C++ prepresentation of a SkPaint
-- createCSkPaint :: SkPaint -> IO CSkPaint
-- createCSkPaint = undefined

newtype SkPaint = SkPaint (ForeignPtr Raw.SkPaint)

withPaint             :: IOE :> es => PaintSpec -> (SkPaint -> Eff es a) -> Eff es a
withPaint paintSpec f = undefined -- newColoredSkPaint (r,g,b) alpha $ \paint -> do
