module Skia.Paint
  ( PaintSpec(PaintSpec)
  , skiaDefaultPaint
  , antiAlias, color, strokeCap, strokeJoin, strokeMiter, strokeWidth

  , AntiAliassed(..)
  , Cap(..)
  , Join(..)
  , PaintStyle(..)

  , SkPaint
  , withPaint
  ) where

import           Control.Lens
import           Data.Colour
import           Data.Default.Class
import           Effectful
import           Foreign.Ptr
import qualified Skia.Canvas.Raw as Raw
import           Skia.Paint.PaintSpec

--------------------------------------------------------------------------------

-- | A (reference to) an Immutable SkPaint value that we can use to paint things.
newtype SkPaint = SkPaint (Ptr Raw.SkPaint)

-- | Draw stuff with the given PaintSpec.
withPaint             :: IOE :> es => PaintSpec -> (SkPaint -> Eff es a) -> Eff es a
withPaint paintSpec f = withEffToIO SeqUnlift $ \unlift ->
                          Raw.withForeignPtr' (Raw.fromPaintSpec paintSpec) $
                            unlift . f . SkPaint
  -- TODO: I think rather than an IOE effect we want "just" a canvas effect.
  -- so maybe we should again use some unsafe wrapping here
