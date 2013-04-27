import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW
import Data.Bits
import Control.Concurrent.STM

import Sphere
import Version
import Renderable

main = do
  initialize
  videoModes <- getVideoModes
  let mode        = last videoModes
      aspectRatio = (fromIntegral $ videoMode_height mode) / (fromIntegral $ videoMode_width mode) :: GLfloat
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
                                     , displayOptions_displayMode    = Window
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
      spheres         <- mapM (createSphere 4 aspectRatio) [(4.0 * cos a, 4.0 * sin a, 13.0) | a <- [0.55,1.0..(pi * 2.0)]]
      tSpheres        <- newTVarIO spheres
      tUpdateSunAngle <- newTVarIO False
      enableKeyRepeat
      setKeyCallback $ keypress tSpheres tUpdateSunAngle
      windowLoop tSpheres tUpdateSunAngle

keypress spheres _ (CharKey '=') True = dispatch spheres (alterShininess   0.01)
keypress spheres _ (CharKey '-') True = dispatch spheres (alterShininess (-0.01))
keypress spheres _ (CharKey '0') True = dispatch spheres (setSpecular 0)
keypress spheres _ (CharKey '1') True = dispatch spheres (setSpecular 1)
keypress spheres _ (CharKey '2') True = dispatch spheres (setSpecular 2)
keypress spheres _ (CharKey 'G') True = do
  when shiftPressed    $ dispatch spheres (alterGamma (-0.05))
  when shiftNotPressed $ dispatch spheres (alterGamma   0.05)
keypress spheres _ (CharKey 'R') True = do
  when shiftPressed    $ dispatch spheres (alterRange (-0.05))
  when shiftNotPressed $ dispatch spheres (alterRange   0.05)
keypress _ updateSunAngle KeyEnter True = atomically $ modifyTVar' updateSunAngle not
keypress _ _ _ _ = return ()

dispatch spheres f = do
  when (keyIsPressed $ CharKey 'E') $ modifyEven spheres f
  when (keyIsPressed $ CharKey 'O') $ modifyOdd  spheres f
  when (keyIsPressed $ CharKey 'A') $ modifyAll  spheres f

shiftPressed :: IO Bool
shiftPressed = do
  left  <- keyIsPressed KeyLeftShift
  right <- keyIsPressed KeyRightShift
  return $ left || right

shiftNotPressed :: IO Bool
shiftNotPressed = do
  pressed <- shiftPressed
  return $ not pressed

alterShininess amount sphere = sphere { shininess = shininess sphere + amount }
alterGamma     amount sphere = sphere { gamma     = gamma sphere + amount }
alterRange     amount sphere = sphere { range     = range sphere + amount }
setSpecular    number sphere = sphere { specular  = number }

windowLoop tSpheres tUpdateAngle = do
  when windowIsOpen $ do
    spheres <- readTVarIO tSpheres
    glLoadIdentity
    glClearColor 0 0 0 1
    glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
    mapM_ render spheres
    swapBuffers
    when (readTVarIO tUpdateAngle) $ modifyAll tSpheres updateSphereSunAngle
    windowLoop tSpheres tUpdateAngle

clamp max value | value > max = 0
                | otherwise   = value
updateSphereSunAngle sphere = sphere { sphereSunAngle = clamp (2.0 * pi) $ sphereSunAngle sphere + 0.02 }

when predicate action = do
  shouldRun <- predicate
  case shouldRun of
    True  -> action
    False -> return ()

modifyEven spheres f = atomically $ modifyTVar' spheres $ alterEven f
modifyOdd  spheres f = atomically $ modifyTVar' spheres $ alterOdd  f
modifyAll  spheres f = atomically $ modifyTVar' spheres $ alterAll  f

alterAll f spheres = map f spheres

alterEven f (a:b:rest) = a : f b : alterEven f rest
alterEven _ rest       = rest

alterOdd f (a:b:rest) = f a : b : alterOdd f rest
alterOdd f (a:[])     = f a : []
alterOdd _ rest       = rest
