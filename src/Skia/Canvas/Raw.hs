module Skia.Canvas.Raw
  ( clear
  , drawPath
  ) where

import Skia.Canvas.Type
import Skia.Color
import Skia.Paint
import Skia.Path

--------------------------------------------------------------------------------

clear :: SkColor -> SkCanvas -> IO ()
clear = undefined


drawPath :: SkCanvas -> SkPath -> SkPaint -> IO ()
drawPath = undefined
