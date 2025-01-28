{-# LANGUAGE TemplateHaskell #-}
module Skia.Paint.PaintSpec
  ( PaintSpec(PaintSpec)
  , skiaDefaultPaint
  , antiAlias, color, strokeCap, strokeJoin, strokeMiter, strokeWidth, style

  , AntiAliassed(..)
  , Cap(..)
  , Join(..)
  , PaintStyle(..)

  ) where

import           Control.Lens
import           Data.Colour
import           Data.Default.Class

--------------------------------------------------------------------------------


data AntiAliassed = NoAntiAliassing | WithAntiAliassing
  deriving (Show,Read,Eq)

instance Default AntiAliassed where
  def = WithAntiAliassing

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


data PaintStyle = FillOnly | StrokeOnly | StrokeAndFill
  deriving (Show,Read,Eq)

instance Default PaintStyle where
  def = StrokeAndFill


-- | Paint specification.
data PaintSpec = PaintSpec { _antiAlias   :: AntiAliassed
                           , _color       :: AlphaColour Float
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
  def = skiaDefaultPaint { _antiAlias = def
                         }

-- | The default paint style in Skia
skiaDefaultPaint :: PaintSpec
skiaDefaultPaint = PaintSpec { _antiAlias   = NoAntiAliassing
                             , _color       = opaque black
                             , _strokeCap   = def
                             , _strokeJoin  = def
                             , _strokeMiter = 0 -- ?
                             , _strokeWidth = 1
                             , _style       = def
                             }
