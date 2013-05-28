import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW
import Data.Bits
import Codec.Image.DevIL
import System.Random

import Version
import Renderable
import Utilities
import Bezier
import Texture
import Uniform
import Job
import Shader

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
      jepsen  <- textureFromImage "jepsen" 555 312
      program <- createProgram "bezier"
      a       <- mapM (createCurve jepsen program) [((x, -1), (x, -0.5), (x, 0)) | x <- [-1.0, -0.995..1.0]]
      force   <- createUniform program $ UniformFloatDescription "force" 0.0
      windowLoop a force 0

createCurve image program (a, b, (x, _)) = do
  y <- randomRIO (-0.2, 0.0)
  strokedBezier a b (x, y) program image

windowLoop items forceUniform force = do
  when windowIsOpen $ do
    glClearColor 1 1 1 1
    glBindFramebuffer gl_FRAMEBUFFER 0
    glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
    glUniform1f (uid forceUniform) $ sin force / 4.0
    mapM_ render items
    swapBuffers
    windowLoop items forceUniform (force + 0.02)

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
