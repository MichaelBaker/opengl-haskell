module Texture where

import Foreign
import Graphics.Rendering.OpenGL.Raw
import Image

textureFromImage filename width height = do
  imageData <- readPNGToBytes $ concat ["../../images/", filename, ".png"]
  glActiveTexture gl_TEXTURE0
  texturePointer <- malloc ::IO (Ptr GLuint)
  glGenTextures 1 texturePointer
  textureId  <- peek texturePointer
  valueArray <- newArray imageData

  glBindTexture gl_TEXTURE_2D textureId

  glTexImage2D    gl_TEXTURE_2D 0 (fromIntegral gl_RGBA8) width height 0 gl_RGBA gl_UNSIGNED_BYTE valueArray
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_BASE_LEVEL 0
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAX_LEVEL  0
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER (fromIntegral gl_LINEAR)
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER (fromIntegral gl_NEAREST)
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S (fromIntegral gl_CLAMP_TO_EDGE)

  glBindTexture gl_TEXTURE_2D 0
  return textureId

create2DTexture = do
  glActiveTexture gl_TEXTURE0
  texturePointer <- malloc ::IO (Ptr GLuint)
  glGenTextures 1 texturePointer
  textureId  <- peek texturePointer
  valueArray <- newArray values

  glBindTexture gl_TEXTURE_2D textureId

  glTexImage2D    gl_TEXTURE_2D 0 (fromIntegral gl_R8) 2 2 0 gl_RED gl_FLOAT valueArray
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_BASE_LEVEL 0
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAX_LEVEL  0
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER (fromIntegral gl_NEAREST)
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER (fromIntegral gl_NEAREST)
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S (fromIntegral gl_CLAMP_TO_EDGE)

  glBindTexture gl_TEXTURE_2D 0
  return textureId

values = [  0.0,  0.33, 0.66, 1.0] :: [GLfloat]
