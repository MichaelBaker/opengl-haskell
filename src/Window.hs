module Window(startWindow) where

import Prelude hiding            (mapM_)
import Control.Concurrent.STM    (TVar, readTVarIO)
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Draw                      (Draw, draw)

-- This must be the last function called in the main thread.
-- OpenGL REALLY wants to be on the main thread and mainLoop blocks forever.
startWindow :: (Draw a) => String -> TVar [a] -> IO ()
startWindow windowName layers = do
  getArgsAndInitialize
  createWindow windowName

  clearColor    $= Color4 1 1 1 1
  blend         $= Enabled
  blendFunc     $= (SrcAlpha, OneMinusSrcAlpha)
  lineSmooth    $= Enabled
  pointSmooth   $= Enabled
  polygonSmooth $= Enabled

  displayCallback $= display layers
  idleCallback    $= Just idle

  mainLoop

display :: (Draw a) => TVar [a] -> IO ()
display layers = do
  clear [ ColorBuffer, DepthBuffer ]
  color (Color4 0 0 0 1 :: Color4 GLfloat)
  layers <- readTVarIO layers
  draw layers
  flush

idle = postRedisplay Nothing
