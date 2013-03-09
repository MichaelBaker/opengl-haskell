import Control.Concurrent.STM (newTVarIO, atomically, writeTVar)
import Control.Concurrent     (forkIO, threadDelay)
import Window                 (startWindow)
import Draw

daCube = colorFaces $ cube 0.9

colorFaces faces = map colorize $ zip faces colors

colors = cycle [0.1, 0.2..0.9]

colorize :: (a, Float) -> Color a
colorize (a, color) = Color color a

main = do
  layers <- newTVarIO [rotate X 0 $ rotate Y 0 daCube]
  forkIO $ spin layers 0.0 0.0 daCube
  startWindow "Ohai" layers

spin store x y cube = do
  atomically $ writeTVar store [rotate X x $ rotate Y y cube]
  threadDelay 30000
  spin store (x + 5) (y + 3) cube

