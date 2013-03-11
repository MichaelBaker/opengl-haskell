module Window(startWindow) where

import Prelude hiding            (mapM_)
import Control.Concurrent.STM    (TVar, readTVarIO)
import Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.OpenGL as G
import Graphics.UI.GLUT
import Draw

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
  polygonMode   $= (Fill, Fill)
  frontFace     $= CCW

  displayCallback $= display layers
  idleCallback    $= Just idle

  mainLoop

display :: (Draw a) => TVar [a] -> IO ()
display layers = do
  clear [ ColorBuffer, DepthBuffer ]
  G.color (Color4 0 0 0 1 :: Color4 GLfloat)

  layers <- readTVarIO layers
  depthFunc $= Just Less
  draw layers
  depthFunc $= Nothing
  draw Coordinates
  depthFunc $= Just Less

  flush

idle = postRedisplay Nothing
