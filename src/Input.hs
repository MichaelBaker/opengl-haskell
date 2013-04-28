module Input where

import Control.Concurrent.STM
import Graphics.UI.GLFW
import Sphere
import Utilities

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

modifyEven spheres f = atomically $ modifyTVar' spheres $ alterEven f
modifyOdd  spheres f = atomically $ modifyTVar' spheres $ alterOdd  f
modifyAll  spheres f = atomically $ modifyTVar' spheres $ alterAll  f

alterAll f spheres = map f spheres

alterEven f (a:b:rest) = a : f b : alterEven f rest
alterEven _ rest       = rest

alterOdd f (a:b:rest) = f a : b : alterOdd f rest
alterOdd f (a:[])     = f a : []
alterOdd _ rest       = rest
