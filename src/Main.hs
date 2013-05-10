import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW
import Data.Bits
import Codec.Image.DevIL

import Version
import Renderable
import Utilities
import Bezier

main = do
  initialize
  ilInit
  videoModes <- getVideoModes

  createWindow $ last videoModes
  initializeSettings
  glfwVersion <- getGlfwVersion
  glVersion   <- getGlVersion

  case checkVersions glfwVersion glVersion of
    (Left  error)    -> putStrLn error
    (Right versions) -> do
      putStrLn $ "[VideoMode] " ++ (show $ last videoModes)
      putStrLn versions
      circle <- quadraticBezier (0, -1, 0) (0, 1, 0) (-1, 1, 0)
      windowLoop [circle]

windowLoop items = do
  when windowIsOpen $ do
    glClearColor 1 1 1 1
    glBindFramebuffer gl_FRAMEBUFFER 0
    glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
    mapM_ render items
    swapBuffers
    windowLoop items

createWindow mode = openWindow $ defaultDisplayOptions { displayOptions_numRedBits     = videoMode_numRedBits   mode
                                                       , displayOptions_numGreenBits   = videoMode_numGreenBits mode
                                                       , displayOptions_numBlueBits    = videoMode_numBlueBits  mode
                                                       , displayOptions_width          = 600
                                                       , displayOptions_height         = 600
                                                       , displayOptions_numAlphaBits   = 8
                                                       , displayOptions_numStencilBits = 8
                                                       , displayOptions_numDepthBits   = 8
                                                       , displayOptions_numFsaaSamples = Just 8
                                                       , displayOptions_displayMode    = Window
                                                       }

initializeSettings = do
  glEnable gl_DEPTH_TEST
  glViewport 0 0 600 600
  glEnable    gl_BLEND
  glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
  setWindowTitle "Ohai"
