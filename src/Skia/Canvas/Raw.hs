{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Skia.Canvas.Raw
  ( testRaw, testDraw

  -- * Main rendering operations we can do
  , withPNGCanvas
  , withSVGCanvas
  , withOpenGLCanvas


  -- * The Canvas type
  , SkCanvas


  -- * Operations we can do on a canvas
  , clear
  , drawPath

  -- * Constructing Paints
  , SkPaint
  , newSkPaint
  , newColoredSkPaint

  -- ** Setting properties of a Paint
  , setAntiAlias
  , setStroke

  -- * Constructing Paths
  , SkPath
  , rectXYWH
  , lineSegment
  , polygon
  , polyLine

  -- * Constructing colors
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
import qualified Data.Vector.Storable as Storable
import           System.OsString.Internal.Types ( getOsString, getPosixString )
import           Data.ByteString (ByteString)
import           System.OsString.Data.ByteString.Short (fromShort)
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


C.context $ C.cppCtx <> C.funCtx <> C.vecCtx <> C.bsCtx <> C.cppTypePairs
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

-- for the SDL integration
C.include "include/gpu/ganesh/GrDirectContext.h"
C.include "include/gpu/ganesh/gl/GrGLInterface.h"
C.include "include/gpu/ganesh/SkSurfaceGanesh.h"
C.include "include/gpu/ganesh/gl/GrGLDirectContext.h"


--------------------------------------------------------------------------------




--------------------------------------------------------------------------------

sk_ColorWHITE :: SkColor
sk_ColorWHITE = [C.pure|unsigned int {SK_ColorWHITE}|]
-- SkColor is an alias for unisgned int





--------------------------------------------------------------------------------
-- * Paints

-- | Create  a new default skPaint
newSkPaint :: IO (ForeignPtr SkPaint)
newSkPaint = newForeignPtr deleteSkPaint =<< newSkPaint'
  where
    newSkPaint' :: IO (Ptr SkPaint)
    newSkPaint' = [C.block|SkPaint* { return new SkPaint;} |]

-- | Creates a new colored paint; the colors are assumed to be in the sRGB color space.
-- (the forth float is the alpha value)
newColoredSkPaint               :: (C.CFloat,C.CFloat,C.CFloat) -> C.CFloat
                                -> IO (ForeignPtr SkPaint)
newColoredSkPaint (r,g,b) alpha = newForeignPtr deleteSkPaint =<< newColoredSkPaint'
  where
    newColoredSkPaint' =
      [C.block|SkPaint* { return new SkPaint({$(float r),$(float g),$(float b),$(float alpha)});}|]

-- | Finalizer for the path
deleteSkPaint :: FunPtr (Ptr SkPaint -> IO ())
deleteSkPaint = [C.funPtr| void deletePaint(SkPaint* paint) { delete paint; } |]


----------------------------------------
-- * Setting Paint attributes

-- setAlpha
-- setAlphaf

-- | Set whether to use AA
setAntiAlias                :: C.CBool -> Ptr SkPaint -> IO ()
setAntiAlias enableAA paint =
  [C.block|void { $(SkPaint* paint)->setAntiAlias($(bool enableAA)); }|]

-- setARGB
-- setBlender
-- setBlendMode
-- setColor
-- setColor
-- setColor4f
-- setColorFilter
-- setDither
-- setImageFilter
-- setMaskFilter
-- setPathEffect
-- setShader

-- | True = stroke only, false = fill only
setStroke                    :: C.CBool -> Ptr SkPaint -> IO ()
setStroke enableStroke paint =
  [C.block|void { $(SkPaint* paint)->setStroke($(bool enableStroke)); }|]
-- TODO
{-
setStrokeCap
setStrokeJoin
setStrokeMiter
setStrokeWidth
setStyle
-}

-- data PaintSpec = PaintSpec { antiAliassed :: Bool
--                            ,
--                            }


--------------------------------------------------------------------------------
-- * Paths

-- | Create a new path
rectXYWH         :: C.CFloat -> C.CFloat -> C.CFloat -> C.CFloat -> IO (ForeignPtr SkPath)
rectXYWH x y w h = newForeignPtr deleteSkPath =<< rectXYWH'
  where
    rectXYWH' = [C.block|
      SkPath* {
          return new SkPath (SkPath::Rect(SkRect::MakeXYWH($(float x), $(float y), $(float w), $(float h))));
      }|]

-- | Create a line segment from s to t
lineSegment                 :: (C.CFloat, C.CFloat) -> (C.CFloat, C.CFloat)
                            -> IO (ForeignPtr SkPath)
lineSegment (sx,sy) (tx,ty) = newForeignPtr deleteSkPath =<< lineSegment'
  where
    lineSegment' = [C.block|
      SkPath* {
        SkPath* path = new SkPath;
        path->moveTo($(float sx), $(float sy));
        path->lineTo($(float tx), $(float ty));

        return path;
      }
      |]

-- | Create a poline
--
-- pre: the vectors both have the same number n>= 1 of elements in them
polyLine                :: Storable.Vector C.CFloat -- ^ xCoords
                        -> Storable.Vector C.CFloat -- ^ y coords
                        -> IO (ForeignPtr SkPath)
polyLine xCoords yCoords = newForeignPtr deleteSkPath =<< polyLine'
  where
    polyLine' = [C.block|
      SkPath* {
        SkPath* path = new SkPath;
        path->moveTo($vec-ptr:(float *xCoords)[0],$vec-ptr:(float *yCoords)[0]);
        for (int i = 1 ; i < $vec-len:xCoords ; i++)
          path->lineTo($vec-ptr:(float *xCoords)[i],$vec-ptr:(float *yCoords)[i]);

        return path;
      }
      |]

-- |
-- Create a polygon
--
-- pre: the vectors both have the same number n>= 1 of elements in them
polygon                 :: Storable.Vector C.CFloat -- ^ xCoords
                        -> Storable.Vector C.CFloat -- ^ y coords
                        -> IO (ForeignPtr SkPath)
polygon xCoords yCoords = newForeignPtr deleteSkPath =<< polygon'
  where
    polygon' =
      [C.block|
        SkPath* {
          SkPath* path = new SkPath;
          path->moveTo($vec-ptr:(float *xCoords)[0],$vec-ptr:(float *yCoords)[0]);
          for (int i = 1 ; i < $vec-len:xCoords ; i++)
            path->lineTo($vec-ptr:(float *xCoords)[i],$vec-ptr:(float *yCoords)[i]);
          path->close();

          return path;
      }
      |]


-- | Finalizer for the path
deleteSkPath :: FunPtr (Ptr SkPath -> IO ())
deleteSkPath = [C.funPtr| void deletePath(SkPath* path) { delete path; } |]

--------------------------------------------------------------------------------

-- | Clears the canvas
clear              :: Ptr SkCanvas -> SkColor -> IO ()
clear canvas color =
  [C.block|void {$(SkCanvas* canvas)->clear($(unsigned int color));}|]

-- | Draws a path on the canvas
drawPath                   :: Ptr SkCanvas -> Ptr SkPath -> Ptr SkPaint -> IO ()
drawPath canvas path paint =
  [C.block|void {
          $(SkCanvas* canvas)->drawPath( *$(SkPath* path), *$(SkPaint* paint) );
  }|]

--------------------------------------------------------------------------------

-- | Renders to an PNG File
withPNGCanvas                   :: C.CInt -- ^ Width of the canvas
                                -> C.CInt -- ^ Height of the canvas
                                -> OsPath -- ^ output path to the png file
                                -> (Ptr SkCanvas -> IO ()) -- ^ drawing function
                                -> IO ()
withPNGCanvas width height filePath draw =
    [C.block|void {
          sk_sp<SkSurface> rasterSurface =
                  SkSurfaces::Raster(SkImageInfo::MakeN32Premul($(int width), $(int height)));

          SkCanvas* canvas = rasterSurface->getCanvas();
          $fun:(void (*draw)(SkCanvas*))(canvas);

          SkPixmap pixmap;
          rasterSurface->peekPixels(&pixmap);

          SkFILEWStream output($bs-ptr:rawFilePath);
          if (!SkPngEncoder::Encode(&output, pixmap, {})) {
            std::cout << "PNG encoding failed.\n";
            return;
          }
    }|]
  where
    rawFilePath :: ByteString
    rawFilePath = fromShort . getPosixString . getOsString $ filePath
    -- TODO: I think this works only on posix for now.
    -- on windows should be s.t. like getWindowsString instead I think?

--------------------------------------------------------------------------------

-- | Can draw to an SVG canvas.
withSVGCanvas                   :: C.CInt -- ^ Width of the canvas
                                -> C.CInt -- ^ Height of the canvas
                                -> OsPath -- ^ output path to the png file
                                -> (Ptr SkCanvas -> IO ()) -- ^ drawing function
                                -> IO ()
withSVGCanvas width height filePath draw =
    [C.block|void {
          SkFILEWStream svgStream($bs-ptr:rawFilePath);

          SkRect bounds = SkRect::MakeIWH($(int width), $(int height));
          std::unique_ptr<SkCanvas> svgCanvas = SkSVGCanvas::Make(bounds, &svgStream);

          SkCanvas* canvas = svgCanvas.get();
          $fun:(void (*draw)(SkCanvas*))(canvas);
    }|]
  where
    rawFilePath :: ByteString
    rawFilePath = fromShort . getPosixString . getOsString $ filePath
    -- TODO: I think this works only on posix for now.
    -- on windows should be s.t. like getWindowsString instead I think?

--------------------------------------------------------------------------------

-- |
-- pre: You've already created your OpenGL context and bound it.
withOpenGLCanvas                   :: C.CInt -- ^ Width of the canvas
                                   -> C.CInt -- ^ Height of the canvas
                                   -> (Ptr SkCanvas -> IO ()) -- ^ drawing function
                                   -> IO ()
withOpenGLCanvas width height draw =
    [C.block|void {
       std::cout << "withOPENGL" << std::endl;
       sk_sp<const GrGLInterface> interface = nullptr;
       sk_sp<GrDirectContext> context = GrDirectContexts::MakeGL(interface);
       SkImageInfo info = SkImageInfo:: MakeN32Premul($(int width), $(int height));
       sk_sp<SkSurface> surface(SkSurfaces::RenderTarget(context.get(), skgpu::Budgeted::kNo, info));
       if (!surface) {
         std::cout << "ERROR, surface is null";
         return;
       }
       std::cout << "DRAWING" << surface << std::endl;
       SkCanvas* canvas = surface->getCanvas();
       $fun:(void (*draw)(SkCanvas*))(canvas);
    }|]
--------------------------------------------------------------------------------

testRaw :: IO ()
testRaw = do withPNGCanvas 255 255 [osp|/tmp/skiaTestImage.png|] testDraw
             withSVGCanvas 500 500 [osp|/tmp/skiaTestImage.svg|] testDraw

-- | Helper of withForeignPtr
withForeignPtr'          :: IO (ForeignPtr a) -> (Ptr a -> IO b) -> IO b
withForeignPtr' create f = do fPtr <- create
                              withForeignPtr fPtr f

testDraw         :: Ptr SkCanvas -> IO ()
testDraw canvas  = do
  withForeignPtr' (rectXYWH 10 20 120 130) $ \rect -> do
    withForeignPtr' (polygon (Storable.fromList [200, 210, 210, 200])
                             (Storable.fromList [200, 200, 210, 210])
                    ) $ \poly ->
      withForeignPtr' (polyLine (Storable.fromList [200, 210, 210])
                                (Storable.fromList [100, 100, 110])) $ \polyL ->
        withForeignPtr' (lineSegment (0,5) (200,10)) $ \seg -> do

          clear canvas sk_ColorWHITE
          withForeignPtr' newSkPaint $ \paint -> do
            drawPath canvas rect paint
            drawPath canvas poly paint

          withForeignPtr' (newColoredSkPaint (0.5,0.6,1) 1.0) $ \paint ->  do
            setAntiAlias (C.CBool 1) paint
            setStroke    (C.CBool 1) paint

            drawPath canvas seg   paint
            drawPath canvas polyL paint

  -- [C.block|void {
  --    delete $(SkPath* rect);
  --    delete $(SkPath* poly);
  --    delete $(SkPath* polyL);
  --    delete $(SkPath* seg);
  -- }|]

  -- deleteSkPath rect
  -- deleteSkPaint paint


  -- path' <- skPathWith (rectXYWH 10 20 120 130)
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

-- testRawImpl                   :: C.CInt -> C.CInt
--                               -> (Ptr SkCanvas -> IO ())
--                               -> IO ()
-- testRawImpl width height draw =
--   [C.block|void {
--               const char pngFilePath[] = "/tmp/skiaTestImage.png";

--               sk_sp<SkSurface> rasterSurface =
--                   SkSurfaces::Raster(SkImageInfo::MakeN32Premul($(int width), $(int height)));

--               SkCanvas* canvas = rasterSurface->getCanvas();
--               $fun:(void (*draw)(SkCanvas*))(canvas);

--               SkPixmap pixmap;
--               rasterSurface->peekPixels(&pixmap);

--               SkFILEWStream output(pngFilePath);
--               if (!SkPngEncoder::Encode(&output, pixmap, {})) {
--                 std::cout << "PNG encoding failed.\n";
--                 return;
--               }


--               std::cout << "Hello world \n";
--           }|]
