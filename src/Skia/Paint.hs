{-# LANGUAGE TemplateHaskell #-}
module Skia.Paint
  ( Paint, antiAlias

  , skPaint


  , SkPaint
  -- , createCSkPaint
  ) where

import           Control.Lens
import           Foreign.Ptr
import qualified Skia.Canvas.Raw as Raw

--------------------------------------------------------------------------------

data Paint = Paint { _antiAlias :: Bool
                   }
           deriving (Show,Eq)


makeLenses ''Paint

-- | Creates a default SkPaint
skPaint = Paint { _antiAlias = False }


-- -- | Creates the C++ prepresentation of a SkPaint
-- createCSkPaint :: SkPaint -> IO CSkPaint
-- createCSkPaint = undefined

newtype SkPaint = SkPaint (Ptr Raw.SkPaint)
