module RenderBuffer where

import Foreign
import Graphics.Rendering.OpenGL.Raw

createRenderBuffer width height = do
  bufferPointer <- malloc :: IO (Ptr GLuint)
  glGenRenderbuffers 1 bufferPointer
  bufferId <- peek bufferPointer
  glBindRenderbuffer gl_RENDERBUFFER bufferId
  glRenderbufferStorage gl_RENDERBUFFER gl_DEPTH_COMPONENT (fromIntegral width) (fromIntegral height)
  glBindRenderbuffer gl_RENDERBUFFER 0
  return bufferId
