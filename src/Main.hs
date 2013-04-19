import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW
import Control.Monad
import Data.Bits
import Control.Concurrent.STM

import Cube
import Sphere
import Version
import Renderable

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
      cubes    <- mapM (createCube "perspective-2") [(-2, z) | z <- [1,4..30]]
      tCubes   <- newTVarIO cubes
      spheres  <- mapM (createSphere 3) [(2, z) | z <- [1,4..30]]
      tSpheres <- newTVarIO spheres
      setKeyCallback $ monitor tCubes
      windowLoop tCubes tSpheres

windowLoop tCubes tSpheres = do
  continue <- windowIsOpen
  when continue $ do
    cubes   <- readTVarIO tCubes
    spheres <- readTVarIO tSpheres
    glLoadIdentity
    glClearColor 1 1 1 1
    glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
    mapM_ render cubes
    mapM_ render spheres
    swapBuffers
    atomically $ modifyTVar' tCubes   (map updateSunAngle)
    atomically $ modifyTVar' tSpheres (map updateSphereSunAngle)
    windowLoop tCubes tSpheres

updateSunAngle       cube   = cube   { sunAngle       = sunAngle cube + 0.01 }
updateSphereSunAngle sphere = sphere { sphereSunAngle = sphereSunAngle sphere + 0.01 }

monitor cubes (CharKey '=') True  = do
  (vIsPressed, hIsPressed) <- selectorStatuses
  when vIsPressed $ changevFov cubes (pi/60.0)
  when hIsPressed $ changehFov cubes (pi/60.0)
monitor cubes (CharKey '-') True  = do
  (vIsPressed, hIsPressed) <- selectorStatuses
  when vIsPressed $ changevFov cubes (-pi/60.0)
  when hIsPressed $ changehFov cubes (-pi/60.0)
monitor _ _ _ = return ()

selectorStatuses = do
  vIsPressed <- keyIsPressed $ CharKey 'V'
  hIsPressed <- keyIsPressed $ CharKey 'H'
  return (vIsPressed, hIsPressed)

changevFov cubes amount = atomically $ modifyTVar' cubes $ map (`modifyvfov` (+ amount))
changehFov cubes amount = atomically $ modifyTVar' cubes $ map (`modifyhfov` (+ amount))
