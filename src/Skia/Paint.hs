module Skia.Paint
  ( PaintSpec(PaintSpec)
  , skiaDefaultPaint
  , antiAlias, color, strokeCap, strokeJoin, strokeMiter, strokeWidth

  , AntiAliassed(..)
  , Cap(..)
  , Join(..)
  , PaintStyle(..)

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
import           Skia.Paint.PaintSpec

--------------------------------------------------------------------------------


-- -- | Creates the C++ prepresentation of a SkPaint
-- createCSkPaint :: SkPaint -> IO CSkPaint
-- createCSkPaint = undefined

newtype SkPaint = SkPaint (ForeignPtr Raw.SkPaint)

withPaint             :: IOE :> es => PaintSpec -> (SkPaint -> Eff es a) -> Eff es a
withPaint paintSpec f = undefined -- newColoredSkPaint (r,g,b) alpha $ \paint -> do
