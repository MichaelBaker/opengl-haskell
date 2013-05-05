module Image where

import Codec.Image.DevIL
import Data.Array.Unboxed
import Foreign

-- I will (for the time being) assume RGBA and png ONLY >:|

readPNGToBytes filepath = do
  imageData <- readImage filepath
  return $ elems imageData

writeBytesToPng path filename width height frame ptr = do
  let filepath = concat [path, "/", filename, show frame, ".png"]
  writeImageFromPtr filepath (height, width) ptr
