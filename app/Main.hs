{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Skia.Canvas.Raw (testRaw, withOpenGLCanvas, testDraw)
-- import Skia.Canvas
import qualified SDL
import           SDL ( Event(..)
                     , Keycode(..), EventPayload(..), KeyboardEventData(..), InputMotion(..)
                     , Keysym(..)
                     , ($=)
                     , V2(..)
                     )

import qualified Graphics.Rendering.OpenGL as GL


--------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    testRaw

    SDL.initializeAll
    let openGLCfg = SDL.defaultOpenGL
        windowCfg = SDL.defaultWindow { SDL.windowGraphicsContext = SDL.OpenGLContext openGLCfg
                                      }
        title = "foo"
    window    <- SDL.createWindow title windowCfg
    glContext <- SDL.glCreateContext window
    SDL.glMakeCurrent window glContext

    drawOnce (SDL.windowInitialSize windowCfg)
    print "drawn?"

    SDL.showWindow window
    SDL.glSwapWindow window

    go

    SDL.destroyWindow window
    SDL.quit

  where
    go = do
      e <- SDL.waitEvent
      case e of
        KeyPress _ -> pure ()
        _          -> go


    drawOnce (V2 width height) = do GL.clearColor $= GL.Color4 1 1 1 1
                                    GL.clear [GL.ColorBuffer]
                                    withOpenGLCanvas width height testDraw
    -- drawOnce = do GL.clearColor $= GL.Color4 1 1 1 1
    --               GL.clear [GL.ColorBuffer]


pattern KeyPress         :: Keycode -> Event
pattern KeyPress keyCode <- (simpleKeyPress -> Just keyCode)

simpleKeyPress    :: Event -> Maybe Keycode
simpleKeyPress e = case eventPayload e of
    KeyboardEvent keyboardEvent
      | keyboardEventKeyMotion keyboardEvent == Pressed ->
          Just $ keysymKeycode (keyboardEventKeysym keyboardEvent)
    _ -> Nothing
