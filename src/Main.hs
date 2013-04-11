import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW
import Control.Monad
import Data.Bits

import Version
import Cube
import Renderable

main = do
  initialize
  openWindow $ defaultDisplayOptions { displayOptions_numRedBits     = 8
                                     , displayOptions_numGreenBits   = 8
                                     , displayOptions_numBlueBits    = 8
                                     , displayOptions_numAlphaBits   = 8
                                     , displayOptions_numStencilBits = 8
                                     , displayOptions_numDepthBits   = 24
                                     , displayOptions_width          = 600
                                     , displayOptions_height         = 600
                                     }
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL
  glMatrixMode gl_PROJECTION
  glViewport 0 0 600 600
  setWindowTitle "Ohai"

  glfwVersion <- getGlfwVersion
  glVersion   <- getGlVersion

  case checkVersions glfwVersion glVersion of
    (Left  error)    -> putStrLn error
    (Right versions) -> do
      putStrLn versions
      cube <- createCube
      windowLoop cube

windowLoop cube = do
  continue <- windowIsOpen
  when continue $ do
    glClearColor 1 1 1 1
    glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
    render cube
    swapBuffers
    windowLoop $ cube { angle = (angle cube) + 0.03 }
