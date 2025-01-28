module Skia.Canvas.Type
  ( SkCanvas
  ) where

import           Foreign.Ptr
import qualified Skia.Canvas.Raw as Raw

--------------------------------------------------------------------------------

-- | A SkCanvas
newtype SkCanvas = SkCanvas (Ptr Raw.SkCanvas)
