{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Skia.Canvas.Raw
  ( clear
  , drawPath

  , withPNGCanvas


  , testRaw


  , SkCanvas
  ) where

import Skia.Color
import Skia.Paint
import Skia.Path
import System.OsPath
-- import           Foreign.C.String (withCString)
import           Foreign.Ptr
import qualified Language.C.Inline.Cpp as C

--------------------------------------------------------------------------------

data CSkCanvas

-- data SkCPaint


-- skiaContext :: C.Context
-- skiaContext =


C.context $ C.cppCtx <> C.funCtx <> C.cppTypePairs
 [ ("SkCanvas" , [t|CSkCanvas|])
 ]

 -- skiaContext

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

-- | A SkCanvas
newtype SkCanvas = SkCanvas (Ptr CSkCanvas)

--------------------------------------------------------------------------------

clear :: SkCanvas -> SkColor -> IO ()
clear = undefined


drawPath :: SkCanvas -> SkPath -> CSkPaint -> IO ()
drawPath = undefined


withPNGCanvas                   :: Int -> Int -> OsPath -> (SkCanvas -> IO a) -> IO a
withPNGCanvas w h filePath draw = undefined



--------------------------------------------------------------------------------

testRaw :: IO ()
testRaw = testRawImpl 255 255 testDraw

testDraw         :: Ptr CSkCanvas -> IO ()
testDraw canvas  =
  [C.block|void {
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
          $(SkCanvas* canvas)->clear(SK_ColorWHITE);
          $(SkCanvas* canvas)->drawPath(path, p);
  }|]

testRawImpl                   :: C.CInt -> C.CInt
                              -> (Ptr CSkCanvas -> IO ())
                              -> IO ()
testRawImpl width height draw =
  [C.block|void {
              const char pngFilePath[] = "/tmp/skiaTestImage.png";

              sk_sp<SkSurface> rasterSurface =
                  SkSurfaces::Raster(SkImageInfo::MakeN32Premul($(int width), $(int height)));

              SkCanvas* canvas = rasterSurface->getCanvas();
              $fun:(void (*draw)(SkCanvas*))(canvas);

              SkPixmap pixmap;
              rasterSurface->peekPixels(&pixmap);

              SkFILEWStream output(pngFilePath);
              if (!SkPngEncoder::Encode(&output, pixmap, {})) {
                std::cout << "PNG encoding failed.\n";
                return;
              }


              std::cout << "Hello world \n";
          }|]
