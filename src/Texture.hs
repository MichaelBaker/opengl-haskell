module Texture where

import Foreign
import Graphics.Rendering.OpenGL.Raw

createTexture width height = do
  glActiveTexture gl_TEXTURE0
  texturePointer <- malloc :: IO (Ptr GLuint)
  glGenTextures 1 texturePointer
  textureId <- peek texturePointer
  glBindTexture gl_TEXTURE_2D textureId
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_LINEAR
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S     $ fromIntegral gl_CLAMP_TO_EDGE
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T     $ fromIntegral gl_CLAMP_TO_EDGE
  glTexImage2D gl_TEXTURE_2D 0 (fromIntegral gl_RGBA) (fromIntegral width) (fromIntegral height) 0 (fromIntegral gl_RGBA) gl_UNSIGNED_BYTE nullPtr
  glBindTexture gl_TEXTURE_2D 0
  return textureId
