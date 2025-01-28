module Skia.Canvas
  (

  ) where

import           Effectful
import           Effectful.Dispatch.Static
import           Effectful.Reader.Static
import qualified Skia.Canvas.Raw as Raw
import           Skia.Canvas.Type
import           Skia.Color
import           Skia.Paint
import           Skia.Path
import           System.OsPath

--------------------------------------------------------------------------------

myDrawing :: Draw :> es => Eff es ()
myDrawing = do clear sk_ColorWHITE
               let paint = skPaint
                   path  = rectXYWH 10 20 100 130
               drawPath path paint


--------------------------------------------------------------------------------


-- newtype DrawM a = ()










-- type Rect = ()
-- MakeXYWH


rectXYWH :: Double -> Double -> Double -> Double -> Path
rectXYWH = undefined

type Color = ()

-- | Clear the screen
clear       :: (Draw :> es) => Color -> Eff es ()
clear color = do DrawRep canvas <- getStaticRep
                 unsafeEff_ $ Raw.clear canvas color

-- | Draw a path with the given paint
drawPath            :: Draw :> es => Path -> Paint -> Eff es ()
drawPath path paint = do DrawRep canvas <- getStaticRep
                         unsafeEff_ $ do cPaint <- undefined -- createCPaint paint
                                         Raw.drawPath canvas path cPaint

-- | Draws stuff on a canvas
drawOnCanvas        :: IOE :> es -- not sure I want this yet.
                    => Canvas -> Eff (Draw : es) a -> Eff es a
drawOnCanvas canvas = evalStaticRep (DrawRep canvas)




withPNGCanvas                   :: IOE :> es
                                => Int -> Int -> OsPath -> (Canvas -> Eff es a) -> Eff es a
withPNGCanvas w h filePath draw = undefined





-- type Paint

-- clear :: Color -> Canvas -> m ()
-- clear = undefined


-- drawPath :: Path -> Paint -> Canvas -> m ()
-- drawPath = undefined




-- type SkSurface = ()



-- withPngSurface :: (SkSurface -> m ()) ->


-- type SkImageInfo = ()

-- makeN32Premul :: Int -> Int -> SkImageInfo
-- makeN32Premul = undefined

-- raster :: SkImageInfo -> m SkSurface
-- raster = undefined

-- getCanvas :: SkSurface -> SkCanvas
-- getCanvas = undefined

-- type SkPixmap = ()

-- peekPixels :: SkSurface -> m SkPixmap
-- peekPixels = undefined


-- type SkFileWStream = ()

-- pngEncoderEncode ::
