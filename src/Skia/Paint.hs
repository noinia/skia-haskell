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

import           Effectful
import           Foreign.Ptr
import           Skia.Canvas.Effect
import qualified Skia.Canvas.Raw as Raw
import           Skia.Paint.PaintSpec

--------------------------------------------------------------------------------

-- | A (reference to) an Immutable SkPaint value that we can use to paint things.
newtype SkPaint = SkPaint (Ptr Raw.SkPaint)

-- | Draw stuff with the given PaintSpec.
withPaint           :: Skia :> es => PaintSpec -> (SkPaint -> Eff es a) -> Eff es a
withPaint paintSpec = unsafeAsSkiaEff SkPaint $ Raw.fromPaintSpec paintSpec
