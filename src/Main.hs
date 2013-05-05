import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW
import Data.Bits
import Control.Concurrent.STM
import Codec.Image.DevIL

import Sphere
import Version
import Renderable
import Utilities
import Texture

main = do
  initialize
  ilInit
  videoModes <- getVideoModes
  let mode        = last videoModes
      height      = videoMode_height mode
      width       = videoMode_width  mode
      aspectRatio = (fromIntegral height) / (fromIntegral width) :: GLfloat

  createWindow       mode
  initializeSettings mode
  glfwVersion <- getGlfwVersion
  glVersion   <- getGlVersion

  case checkVersions glfwVersion glVersion of
    (Left  error)    -> putStrLn error
    (Right versions) -> do
      putStrLn $ "[VideoMode] " ++ show mode
      putStrLn versions
      sphere   <- createSphere 4 aspectRatio (0.0, 0.0, 4.0)
      tSpheres <- newTVarIO [sphere]
      windowLoop tSpheres

windowLoop tSpheres = do
  when windowIsOpen $ do
    glClearColor 1 1 1 1
    glBindFramebuffer gl_FRAMEBUFFER 0
    glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
    readTVarIO tSpheres >>= mapM_ render
    glFlush
    glFinish
    swapBuffers
    windowLoop tSpheres

createWindow mode = openWindow $ defaultDisplayOptions { displayOptions_numRedBits     = videoMode_numRedBits   mode
                                                       , displayOptions_numGreenBits   = videoMode_numGreenBits mode
                                                       , displayOptions_numBlueBits    = videoMode_numBlueBits  mode
                                                       , displayOptions_width          = videoMode_width mode
                                                       , displayOptions_height         = videoMode_height mode
                                                       , displayOptions_numAlphaBits   = 8
                                                       , displayOptions_numStencilBits = 8
                                                       , displayOptions_numDepthBits   = 8
                                                       , displayOptions_numFsaaSamples = Just 8
                                                       , displayOptions_displayMode    = Window
                                                       }

initializeSettings mode = do
  glEnable gl_DEPTH_TEST
  glViewport 0 0 (fromIntegral $ videoMode_width mode) (fromIntegral $ videoMode_height mode)
  glEnable    gl_BLEND
  glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
  setWindowTitle "Ohai"
