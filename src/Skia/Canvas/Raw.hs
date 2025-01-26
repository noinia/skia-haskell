{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Skia.Canvas.Raw
  ( clear
  , drawPath

  , withPNGCanvas


  , testRaw
  ) where

import Skia.Canvas.Type
import Skia.Color
import Skia.Paint
import Skia.Path
import System.OsPath

import qualified Language.C.Inline.Cpp as C

--------------------------------------------------------------------------------

C.context $ C.cppCtx

--------------------------------------------------------------------------------
-- * C++ Imports

C.include "<iostream>"

-- Basic SKia imports
C.include "include/core/SkSurface.h"
-- C.include "include/core/SkPath.h"
-- C.include "include/core/SkCanvas.h"
-- C.include "include/core/SkData.h"
-- C.include "include/core/SkImage.h"
-- C.include "include/core/SkStream.h"

-- for the PNG writer
-- C.include "include/encode/SkPngEncoder.h"

-- -- for the svg example
-- C.include "include/svg/SkSVGCanvas.h"
-- C.include "src/xml/SkXMLWriter.h"

--------------------------------------------------------------------------------

clear :: SkColor -> SkCanvas -> IO ()
clear = undefined


drawPath :: SkCanvas -> SkPath -> CSkPaint -> IO ()
drawPath = undefined


withPNGCanvas                   :: Int -> Int -> OsPath -> (SkCanvas -> IO a) -> IO a
withPNGCanvas w h filePath draw = undefined



--------------------------------------------------------------------------------

testRaw :: IO ()
testRaw = [C.block|void {
              std::cout << "Hello world";
          }|]
