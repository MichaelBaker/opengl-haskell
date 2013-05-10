module Utilities where

import Graphics.Rendering.OpenGL.Raw
import Foreign

when predicate action = do
  shouldRun <- predicate
  case shouldRun of
    True  -> action
    False -> return ()

glCheckErrors code | code == gl_INVALID_ENUM                  = error "[Error] Invalid enum"
                   | code == gl_INVALID_VALUE                 = error "[Error] Invalid value"
                   | code == gl_INVALID_OPERATION             = error "[Error] Invalid operation"
                   | code == gl_STACK_OVERFLOW                = error "[Error] Stack overflow"
                   | code == gl_STACK_UNDERFLOW               = error "[Error] Stack underflow"
                   | code == gl_OUT_OF_MEMORY                 = error "[Error] Out of memory"
                   | code == gl_INVALID_FRAMEBUFFER_OPERATION = error "[Error] Invalid framebuffer operation"
                   | code == gl_TABLE_TOO_LARGE               = error "[Error] Table too large"
                   | code == gl_NO_ERROR                      = return ()
                   | otherwise                                = error "[Error] Unknown error code"

listSize list = fromIntegral $ length list * sizeOf (head list)

createBuffer target bufferData size = do
  bufferPtr <- (malloc :: IO (Ptr GLuint))
  glGenBuffers 1 bufferPtr
  buffer <- peek bufferPtr
  glBindBuffer target buffer
  glBufferData target size bufferData gl_STATIC_DRAW
  return buffer

cross (a, b, c, _) (x, y, z, _) = (b*z - c*y, c*x - a*z, a*y - b*x, 1.0)

minus     (a, b, c, _) (x, y, z, _) = (a - x, b - y, c - z, 1.0)
plus      (a, b, c, _) (x, y, z, _) = (a + x, b + y, c + z, 1.0)
midpoint  (a, b, c, _) (x, y, z, _) = ((a+x)/2.0, (b+y)/2.0, (c+z)/2.0, 1.0)
normalize (x, y, z, w)              = (x/l, y/l, z/l, w) where l = sqrt $ (x^2) + (y^2) + (z^2)

detuple (a, b, c, d) = [a, b, c, d]

flattenVerticies = concat . map detuple

glTypeSize glType | glType == gl_FLOAT = sizeOf (0 :: GLfloat)
                  | glType == gl_INT   = sizeOf (0 :: GLint)

glSizeOf count glType = fromIntegral $ (fromIntegral count) * glTypeSize glType
