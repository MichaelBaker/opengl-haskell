import Control.Concurrent.STM (newTVarIO)
import Window                 (startWindow)

main = do
  layers <- newTVarIO ([(10, 50), (100, 100), (100, 0), (10, 50)] :: [(Float, Float)])
  startWindow "Ohai" layers
