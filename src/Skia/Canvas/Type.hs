module Skia.Canvas.Type
  ( Canvas
  , Raw.SkCanvas
  ) where

import           Foreign.Ptr
import qualified Skia.Canvas.Raw as Raw

--------------------------------------------------------------------------------

-- | A SkCanvas
newtype Canvas = Canvas (Ptr Raw.SkCanvas)
