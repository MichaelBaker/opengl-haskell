module FrameBuffer where

import Foreign
import Graphics.Rendering.OpenGL.Raw

createFrameBuffer textureId renderBufferId = do
  bufferPointer <- malloc :: IO (Ptr GLuint)
  glGenFramebuffers 1 bufferPointer
  bufferId <- peek bufferPointer
  glBindFramebuffer gl_FRAMEBUFFER bufferId
  glFramebufferTexture2D    gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D textureId 0
  glFramebufferRenderbuffer gl_FRAMEBUFFER gl_DEPTH_ATTACHMENT  gl_RENDERBUFFER renderBufferId
  status <- glCheckFramebufferStatus gl_FRAMEBUFFER
  verifyFrameBuffer status
  glBindFramebuffer gl_FRAMEBUFFER 0
  return bufferId

verifyFrameBuffer status | status == gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT         = error "[Framebuffer] Failed because of an incomplete attachment"
                         | status == gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS         = error "[Framebuffer] Failed because of incomplete dimensions"
                         | status == gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = error "[Framebuffer] Failed because of a missing attachment"
                         | status == gl_FRAMEBUFFER_UNSUPPORTED                   = error "[Framebuffer] Failed because of it is unsupported"
                         | status == gl_FRAMEBUFFER_COMPLETE                      = putStrLn "[Framebuffer] Created successfully"
                         | otherwise                                              = error "[Framebuffer] There was an error with the framebuffer"
