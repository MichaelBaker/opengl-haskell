import Prelude hiding         ((.), id)
import Control.Concurrent.STM (newTVarIO, atomically, writeTVar)
import Control.Concurrent     (forkIO)
import Control.Wire
import Window                 (startWindow)
import Draw

main = do
  layers <- newTVarIO [map (rotate X 0) $ map (rotate Y 0) daCube]
  forkIO $ spin layers daCube rotation clockSession
  startWindow "Ohai" layers

daCube = colorFaces $ cube 0.9

colorFaces faces = map colorize $ zip faces colors

colors = cycle [0.1, 0.2..0.9]

colorize :: (Face, Float) -> Face
colorize (a, c) = color c a

constant :: Double -> WireP a Double -> WireP a Float
constant initial speed = realToFrac <$> integral_ initial . speed

rotation = liftA2 (,) (constant 0 80) (constant 0 100)

spin store cube rotation session = do
  (value, rotation', session') <- stepSessionP rotation session ()
  update value store cube
  spin store cube rotation' session'

update (Right (x, y)) store cube = atomically $ do
  writeTVar store [
    map (scale Y (abs $ cos x)) $ map (scale X (abs $ sin x)) $ map (translate X 0.5) $ map (rotate X x) $ map (rotate Y y) cube,
    map (rotate Z y) $ map (translate X 0.5) $ map (rotate X x) $ map (rotate Y y)cube
    ]
update _ _ _ = return ()

