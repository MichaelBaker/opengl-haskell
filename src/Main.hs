import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW
import Control.Monad
import Data.Bits
import Control.Concurrent.STM

import Version
import Cube
import Renderable

data RotatingCube = RotatingCube { cube       :: CubeJob
                                 , isRotating :: Bool
                                 }
main = do
  initialize
  openWindow $ defaultDisplayOptions { displayOptions_numRedBits     = 8
                                     , displayOptions_numGreenBits   = 8
                                     , displayOptions_numBlueBits    = 8
                                     , displayOptions_numAlphaBits   = 8
                                     , displayOptions_numStencilBits = 8
                                     , displayOptions_numDepthBits   = 24
                                     , displayOptions_width          = 1200
                                     , displayOptions_height         = 600
                                     }
  glEnable gl_DEPTH_TEST
  glViewport 0 0 1200 600
  setWindowTitle "Ohai"

  glfwVersion <- getGlfwVersion
  glVersion   <- getGlVersion

  case checkVersions glfwVersion glVersion of
    (Left  error)    -> putStrLn error
    (Right versions) -> do
      putStrLn versions

      cubeOrtho       <- createCube "ortho"
      cubePerspective <- createCube "perspective"

      tCubeOrtho       <- newTVarIO $ RotatingCube cubeOrtho       False
      tCubePerspective <- newTVarIO $ RotatingCube cubePerspective False

      setKeyCallback $ monitor tCubeOrtho tCubePerspective

      windowLoop tCubeOrtho tCubePerspective

monitor ortho perspective key isPressed = do
  left  <- keyIsPressed KeyLeftShift
  right <- keyIsPressed KeyRightShift

  case (key, isPressed, left, right) of
    (CharKey 'R', True, True, _   ) -> atomically $ modifyTVar' perspective toggleRotation
    _                               -> return ()

  case (key, isPressed, left, right) of
    (CharKey 'R', True, _,    True) -> atomically $ modifyTVar' ortho toggleRotation
    _                               -> return ()

windowLoop cubeOrtho cubePerspective = do
  continue <- windowIsOpen
  when continue $ do
    ortho       <- readTVarIO cubeOrtho
    perspective <- readTVarIO cubePerspective

    glLoadIdentity
    glClearColor 1 1 1 1
    glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT

    render $ cube perspective
    render $ cube ortho

    swapBuffers

    atomically $ modifyTVar' cubeOrtho       updateAngle
    atomically $ modifyTVar' cubePerspective updateAngle

    windowLoop cubeOrtho cubePerspective

updateAngle rCube | isRotating rCube = rCube { cube = (cubeModel { angle = (angle cubeModel) + 0.03} ) }
                  | otherwise        = rCube
                  where cubeModel = cube rCube

toggleRotation cube | isRotating cube = cube { isRotating = False }
                    | otherwise       = cube { isRotating = True  }
