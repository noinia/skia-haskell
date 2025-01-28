module Skia.Path
  ( SkPath
  , rectXYWH
  , circle
  , lineSegment
  , polyLine
  , polygon
  ) where

import           Data.Bifunctor
import           Data.Foldable1
import           Effectful
import           Foreign.Ptr
import           Skia.Canvas.Effect
import qualified Skia.Canvas.Raw as Raw
import qualified VectorBuilder.Builder as Builder
import           VectorBuilder.Vector (build)

--------------------------------------------------------------------------------

-- | Reference to a Skia path
newtype SkPath = SkPath (Ptr Raw.SkPath)

-- | Creates a skia Path representing a rectangle
rectXYWH         :: (Skia :> es, Real r)
                 => r -> r -> r -> r -> (SkPath -> Eff es a) -> Eff es a
rectXYWH x y w h = unsafeAsSkiaEff SkPath $ Raw.rectXYWH (realToFrac x) (realToFrac y)
                                                         (realToFrac w) (realToFrac h)

-- | Creates a skia Path representing a circle
circle       :: (Skia :> es, Real r)
             => r -> r -> r -> (SkPath -> Eff es a) -> Eff es a
circle x y r = unsafeAsSkiaEff SkPath
             $ Raw.circle (realToFrac x, realToFrac y) (realToFrac r)

-- | Creates a skia Path representing a line segment
lineSegment     :: (Skia :> es, Real r)
                => (r, r) -> (r,r) -> (SkPath -> Eff es a) -> Eff es a
lineSegment s t = unsafeAsSkiaEff SkPath
                $ Raw.lineSegment (convert s) (convert t)
  where
    convert (x,y) = (realToFrac x, realToFrac y)

-- | Creates a skia Path representing a polyline
polyLine      :: (Skia :> es, Real r, Foldable1 nonEmpty)
              => nonEmpty (r,r) -> (SkPath -> Eff es a) -> Eff es a
polyLine poly = unsafeAsSkiaEff SkPath $ Raw.polyLine xCoords yCoords
  where
    (xCoords,yCoords) = bimap build build
                      $ foldMap1 (\(x,y) -> (singleton x, singleton y)) poly
    singleton = Builder.singleton . realToFrac

-- | Creates a skia Path representing a simple polygon
polygon        :: (Skia :> es, Real r, Foldable1 nonEmpty)
               => nonEmpty (r,r) -> (SkPath -> Eff es a) -> Eff es a
polygon poly = unsafeAsSkiaEff SkPath $ Raw.polygon xCoords yCoords
  where
    (xCoords,yCoords) = bimap build build
                      $ foldMap1 (\(x,y) -> (singleton x, singleton y)) poly
    singleton = Builder.singleton . realToFrac
