module Skia.Canvas.Effect
  ( Draw
  , runDraw

  , Skia
  , runSkia

  , unsafeAsSkiaEff
  ) where

import           Effectful
import           Effectful.Dispatch.Static
import           Foreign.ForeignPtr
import           Foreign.Ptr
import qualified Skia.Canvas.Raw as Raw
import           Skia.Canvas.Type

--------------------------------------------------------------------------------

-- | Effect telling us we can draw things
type data Draw :: Effect

type instance DispatchOf Draw = Static NoSideEffects
newtype instance StaticRep Draw = DrawRep SkCanvas

-- | Draw things on the given canvas.
runDraw :: SkCanvas -> Eff (Draw : es) a -> Eff es a
runDraw = evalStaticRep . DrawRep


-- | Effect telling us we can declare/define skia values.
type data Skia :: Effect

type instance DispatchOf Skia = Static NoSideEffects
-- The only effect that the skia operations themselves have is that they allocate memory.
-- so in essense they are pure.

data instance StaticRep Skia = SkiaRep

--------------------------------------------------------------------------------

runSkia :: Eff (Skia : es) a -> Eff es a
runSkia = evalStaticRep SkiaRep

--------------------------------------------------------------------------------

-- | Helper function to wrap the Raw API in a skia api
unsafeAsSkiaEff               :: (Skia :> es)
                              => (Ptr a -> safeA)
                              -> IO (ForeignPtr a)
                              -> (safeA -> Eff es result)
                              -> Eff es result
unsafeAsSkiaEff wrap create f = unsafeSeqUnliftIO $ \unlift ->
                                  Raw.withForeignPtr' create $
                                    unlift . f . wrap
{-# INLINE unsafeAsSkiaEff #-}
