module Skia.Path
  ( Path


  , Raw.SkPath
  ) where


import qualified Skia.Canvas.Raw as Raw
import           Foreign.Ptr

--------------------------------------------------------------------------------

newtype Path = Path (Ptr Raw.SkPath)
