import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW
import Data.Bits
import Control.Concurrent.STM

import Sphere
import Version
import Renderable
import Input
import Utilities
import FrameBuffer
import RenderBuffer
import Texture
import WindowFrame
import Blocks

main = do
  initialize
  videoModes <- getVideoModes
  let mode        = last videoModes
      height      = videoMode_height mode
      width       = videoMode_width mode
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

      spheres         <- mapM (createSphere 4 aspectRatio) spherePositions
      tSpheres        <- newTVarIO spheres
      tUpdateSunAngle <- newTVarIO False

      textureId      <- createTexture width height
      renderBufferId <- createRenderBuffer width height
      frameBufferId  <- createFrameBuffer textureId renderBufferId
      windowFrame    <- createWindowFrame textureId

      enableKeyRepeat
      setKeyCallback $ keypress tSpheres tUpdateSunAngle
      windowLoop tSpheres tUpdateSunAngle frameBufferId textureId windowFrame

windowLoop tSpheres tUpdateAngle frameBufferId textureId windowFrame = do
  when windowIsOpen $ do
    glClearColor 1 1 1 1

    glBindFramebuffer gl_FRAMEBUFFER frameBufferId
    glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
    readTVarIO tSpheres >>= mapM_ render

    glBindFramebuffer gl_FRAMEBUFFER 0
    glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT

    glBindTexture gl_TEXTURE_2D textureId
    glGenerateMipmap gl_TEXTURE_2D

    render windowFrame

    swapBuffers
    when (readTVarIO tUpdateAngle) $ modifyAll tSpheres updateSphereSunAngle
    windowLoop tSpheres tUpdateAngle frameBufferId textureId windowFrame

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
