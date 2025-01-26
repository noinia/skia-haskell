{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Skia.Canvas.Raw
  ( clear
  , drawPath

  , withPNGCanvas


  , testRaw


  , SkCanvas

  , SkPaint

  , SkPath
  , rectXYWH

  , SkColor
  , sk_ColorWHITE
  ) where

-- import Skia.Color
import           System.OsPath
-- import           Foreign.C.String (withCString)
import           Foreign.Ptr
import           Foreign.ForeignPtr
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import           Data.Word

--------------------------------------------------------------------------------

data SkCanvas

-- data SkColor

data SkPath

data SkPaint

-- data SkCPaint

-- type SkColor = Word32
type SkColor = C.CUInt

-- skiaContext :: C.Context
-- skiaContext =


C.context $ C.cppCtx <> C.funCtx <> C.cppTypePairs
 [ ("SkCanvas", [t|SkCanvas|])
 -- , ("SkColor",  [t|SkColor|])
 , ("SkPath",   [t|SkPath|])
 , ("SkPaint",  [t|SkPaint|])
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




--------------------------------------------------------------------------------

sk_ColorWHITE :: SkColor
sk_ColorWHITE = [C.pure|unsigned int {SK_ColorWHITE}|]
-- SkColor is an alias for unisgned int


--------------------------------------------------------------------------------

newSkPaint :: IO (Ptr SkPaint)
newSkPaint = [C.block|SkPaint* { return new SkPaint;}
             |]

-- | Finalizer for the path
deleteSkPaint :: FunPtr (Ptr SkPaint -> IO ())
deleteSkPaint = [C.funPtr| void deletePaint(SkPaint* paint) { delete paint; } |]


--------------------------------------------------------------------------------

-- | Create a new path
rectXYWH         :: C.CFloat -> C.CFloat -> C.CFloat -> C.CFloat -> IO (Ptr SkPath)
rectXYWH x y w h = [C.block|
      SkPath* {
          return new SkPath (SkPath::Rect(SkRect::MakeXYWH($(float x), $(float y), $(float w), $(float h))));
      }|]

-- | Finalizer for the path
deleteSkPath :: FunPtr (Ptr SkPath -> IO ())
deleteSkPath = [C.funPtr| void deletePath(SkPath* path) { delete path; } |]


-- | Create a SkPath with the given constructor
skPath        :: IO (Ptr SkPath) -> IO (ForeignPtr SkPath)
skPath create = create >>= newForeignPtr deleteSkPath

--------------------------------------------------------------------------------


clear              :: Ptr SkCanvas -> SkColor -> IO ()
clear canvas color =
  [C.block|void {$(SkCanvas* canvas)->clear($(unsigned int color));}|]

drawPath :: Ptr SkCanvas -> Ptr SkPath -> Ptr SkPaint -> IO ()
drawPath canvas path paint =
  [C.block|void {
          $(SkCanvas* canvas)->drawPath( *$(SkPath* path), *$(SkPaint* paint) );
  }|]

withPNGCanvas                   :: Int -> Int -> OsPath -> (Ptr SkCanvas -> IO a) -> IO a
withPNGCanvas w h filePath draw = undefined

--------------------------------------------------------------------------------

testRaw :: IO ()
testRaw = testRawImpl 255 255 testDraw

testDraw         :: Ptr SkCanvas -> IO ()
testDraw canvas  = do
  rect  <- rectXYWH 10 20 120 130
  paint <- newSkPaint
  clear canvas sk_ColorWHITE
  drawPath canvas rect paint
  [C.block|void {
     delete $(SkPath* rect);
     delete $(SkPaint* paint);
  }|]

  -- deleteSkPath rect
  -- deleteSkPaint paint


  -- path' <- skPath (rectXYWH 10 20 120 130)
  -- withForeignPtr path' $ \path -> do
  --   clear canvas sk_ColorWHITE
  --   drawPath

  -- [C.block|void {
  --         SkPath path;
  --         path.moveTo(10.0f, 10.0f);
  --         path.lineTo(100.0f, 0.0f);
  --         path.lineTo(100.0f, 100.0f);
  --         path.lineTo(0.0f, 100.0f);
  --         path.lineTo(50.0f, 50.0f);
  --         path.close();

  --         // creating a paint to draw with
  --         SkPaint p;
  --         p.setAntiAlias(true);

  --         // clear out which may be was drawn before and draw the path
  --         $(SkCanvas* canvas)->clear(SK_ColorWHITE);
  --         $(SkCanvas* canvas)->drawPath(path, p);
  -- }|]

testRawImpl                   :: C.CInt -> C.CInt
                              -> (Ptr SkCanvas -> IO ())
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
