import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW
import Data.Bits
import Control.Concurrent.STM

import Sphere
import Version
import Renderable
import Input
import Utilities

main = do
  initialize
  videoModes <- getVideoModes
  let mode        = last videoModes
      aspectRatio = (fromIntegral $ videoMode_height mode) / (fromIntegral $ videoMode_width mode) :: GLfloat
  createWindow       mode
  initializeSettings mode
  glfwVersion <- getGlfwVersion
  glVersion   <- getGlVersion

  case checkVersions glfwVersion glVersion of
    (Left  error)    -> putStrLn error
    (Right versions) -> do
      putStrLn $ "[VideoMode] " ++ show mode
      putStrLn versions

      spheres         <- mapM (createSphere 4 aspectRatio) spherePositions
      tSpheres        <- newTVarIO spheres
      tUpdateSunAngle <- newTVarIO False

      enableKeyRepeat
      setKeyCallback $ keypress tSpheres tUpdateSunAngle
      windowLoop tSpheres tUpdateSunAngle

spherePositions = [(4.0 * cos a, 4.0 * sin a, 13.0) | a <- [0.0,(pi * 2.0)/5.0..(pi * 2.0)]]

createWindow mode = openWindow $ defaultDisplayOptions { displayOptions_numRedBits     = videoMode_numRedBits   mode
                                                       , displayOptions_numGreenBits   = videoMode_numGreenBits mode
                                                       , displayOptions_numBlueBits    = videoMode_numBlueBits  mode
                                                       , displayOptions_width          = videoMode_width        mode
                                                       , displayOptions_height         = videoMode_height       mode
                                                       , displayOptions_numAlphaBits   = 8
                                                       , displayOptions_numStencilBits = 8
                                                       , displayOptions_numDepthBits   = 8
                                                       , displayOptions_displayMode    = Window
                                                       }

initializeSettings mode = do
  glEnable gl_DEPTH_TEST
  glViewport 0 0 (fromIntegral $ videoMode_width mode) (fromIntegral $ videoMode_height mode)
  setWindowTitle "Ohai"

windowLoop tSpheres tUpdateAngle = do
  when windowIsOpen $ do
    glLoadIdentity
    glClearColor 1 1 1 1
    glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
    readTVarIO tSpheres >>= mapM_ render
    swapBuffers
    when (readTVarIO tUpdateAngle) $ modifyAll tSpheres updateSphereSunAngle
    windowLoop tSpheres tUpdateAngle
