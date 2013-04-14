import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW
import Control.Monad
import Data.Bits
import Control.Concurrent.STM

import Version
import Cube (createCube, CubeJob(..))
import Renderable

data CubeView = CubeView { cube :: CubeJob }
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
      cubes  <- mapM (createCube "perspective-2") [(x, z) | z <- [1,4..30], x <- [-2, 2]]
      tCubes <- newTVarIO $ map CubeView cubes
      setKeyCallback $ monitor tCubes
      windowLoop tCubes

windowLoop tCubes = do
  continue <- windowIsOpen
  when continue $ do
    cubes <- readTVarIO tCubes
    glLoadIdentity
    glClearColor 1 1 1 1
    glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
    mapM_ (render . cube) cubes
    swapBuffers
    windowLoop tCubes

monitor cubes (CharKey '=') True  = do
  (vIsPressed, hIsPressed) <- selectorStatuses
  when vIsPressed $ increasevFov cubes (pi/60.0)
  when hIsPressed $ increasehFov cubes (pi/60.0)
monitor cubes (CharKey '-') True  = do
  (vIsPressed, hIsPressed) <- selectorStatuses
  when vIsPressed $ decreasevFov cubes (pi/60.0)
  when hIsPressed $ decreasehFov cubes (pi/60.0)
monitor _ _ _ = return ()

selectorStatuses = do
  vIsPressed <- keyIsPressed $ CharKey 'V'
  hIsPressed <- keyIsPressed $ CharKey 'H'
  return (vIsPressed, hIsPressed)

increasevFov cubes amount = atomically $ modifyTVar' cubes $ map (\c -> c { cube = modifyvfov (cube c) (+ amount)})
decreasevFov cubes amount = atomically $ modifyTVar' cubes $ map (\c -> c { cube = modifyvfov (cube c) (+ (-amount))})

increasehFov cubes amount = atomically $ modifyTVar' cubes $ map (\c -> c { cube = modifyhfov (cube c) (+ amount)})
decreasehFov cubes amount = atomically $ modifyTVar' cubes $ map (\c -> c { cube = modifyhfov (cube c) (+ (-amount))})

modifyvfov cube f = cube { vfov = f (vfov cube) }
modifyhfov cube f = cube { hfov = f (hfov cube) }
