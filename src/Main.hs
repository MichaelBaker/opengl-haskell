import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW
import Control.Monad
import Data.Bits
import Control.Concurrent.STM

import Sphere
import Version
import Renderable

main = do
  initialize
  videoModes <- getVideoModes
  let mode = last videoModes
  putStrLn $ "[VideoMode] " ++ (show mode)
  openWindow $ defaultDisplayOptions { displayOptions_numRedBits     = videoMode_numRedBits   mode
                                     , displayOptions_numGreenBits   = videoMode_numGreenBits mode
                                     , displayOptions_numBlueBits    = videoMode_numBlueBits  mode
                                     , displayOptions_width          = videoMode_width        mode
                                     , displayOptions_height         = videoMode_height       mode
                                     , displayOptions_numAlphaBits   = 8
                                     , displayOptions_numStencilBits = 8
                                     , displayOptions_numDepthBits   = 24
                                     , displayOptions_numFsaaSamples = Just 16
                                     , displayOptions_displayMode    = Fullscreen
                                     }
  glEnable gl_DEPTH_TEST
  glViewport 0 0 (fromIntegral $ videoMode_width mode) (fromIntegral $ videoMode_height mode)
  setWindowTitle "Ohai"

  glfwVersion <- getGlfwVersion
  glVersion   <- getGlVersion

  case checkVersions glfwVersion glVersion of
    (Left  error)    -> putStrLn error
    (Right versions) -> do
      putStrLn versions
      spheres  <- mapM (createSphere 3) [(4.5 * cos a, 4.5 * sin a, z) | a <- [0,0.3..(pi*2)], z <- [8,10..16]]
      tSpheres <- newTVarIO spheres
      windowLoop tSpheres

windowLoop tSpheres = do
  continue <- windowIsOpen
  when continue $ do
    spheres <- readTVarIO tSpheres
    glLoadIdentity
    glClearColor 1 1 1 1
    glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
    mapM_ render spheres
    swapBuffers
    atomically $ modifyTVar' tSpheres (map updateSphereSunAngle)
    windowLoop tSpheres

updateSphereSunAngle sphere = sphere { sphereSunAngle = sphereSunAngle sphere + 0.001 }
