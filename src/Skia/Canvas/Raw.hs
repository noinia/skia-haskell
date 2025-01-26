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
C.include "include/core/SkPath.h"
C.include "include/core/SkCanvas.h"
C.include "include/core/SkData.h"
C.include "include/core/SkImage.h"
C.include "include/core/SkStream.h"

-- for the PNG writer
C.include "include/encode/SkPngEncoder.h"

-- -- for the svg example
C.include "include/svg/SkSVGCanvas.h"
C.include "src/xml/SkXMLWriter.h"

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
              const char pngFilePath[] = "/tmp/skiaTestImage.png";
              int width = 256;
              int height = 256;

              sk_sp<SkSurface> rasterSurface = SkSurfaces::Raster(SkImageInfo::MakeN32Premul(width, height));
              SkCanvas* canvas = rasterSurface->getCanvas();

              SkPath path;
              path.moveTo(10.0f, 10.0f);
              path.lineTo(100.0f, 0.0f);
              path.lineTo(100.0f, 100.0f);
              path.lineTo(0.0f, 100.0f);
              path.lineTo(50.0f, 50.0f);
              path.close();

              // creating a paint to draw with
              SkPaint p;
              p.setAntiAlias(true);

              // clear out which may be was drawn before and draw the path
              canvas->clear(SK_ColorWHITE);
              canvas->drawPath(path, p);

              SkPixmap pixmap;
              rasterSurface->peekPixels(&pixmap);

              SkFILEWStream output(pngFilePath);
              if (!SkPngEncoder::Encode(&output, pixmap, {})) {
                std::cout << "PNG encoding failed.\n";
                return;
              }


              std::cout << "Hello world";
          }|]
