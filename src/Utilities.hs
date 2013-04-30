module Utilities where

import Graphics.Rendering.OpenGL.Raw

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
