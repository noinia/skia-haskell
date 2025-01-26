{-# LANGUAGE TemplateHaskell #-}
module Skia.Paint
  ( SkPaint, antiAlias

  , skPaint


  , CSkPaint
  , createCSkPaint
  ) where

import Control.Lens

--------------------------------------------------------------------------------

type CSkPaint = ()


data SkPaint = SkPaint { _antiAlias :: Bool
                       }
             deriving (Show,Eq)


makeLenses ''SkPaint

-- | Creates a default SkPaint
skPaint = SkPaint { _antiAlias = False }




-- | Creates the C++ prepresentation of a SkPaint
createCSkPaint :: SkPaint -> IO CSkPaint
createCSkPaint = undefined
