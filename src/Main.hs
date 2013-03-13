import Control.Monad
import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Control.Applicative

verticies :: [GLfloat]
verticies = [ 0.75, 0.75, 0.0, 1.0,
              0.75, (-0.75), 0.0, 1.0,
              (-0.75), (-0.75), 0.0, 1.0 ]

main = do
  initialize
  openWindow defaultDisplayOptions
  glClearColor 0 0 0 0
  alloca $ \ptr -> do
    glGenBuffers 1 ptr
    positionBuffer <- peek ptr
    loop positionBuffer
    glDeleteBuffers 1 ptr

loop buffer = do
  continue <- windowIsOpen
  when continue $ do
    glClear gl_COLOR_BUFFER_BIT

    glBindBuffer gl_ARRAY_BUFFER buffer

    withArray verticies $ \ary -> do
      let bytes = fromIntegral $ length verticies * sizeOf (head verticies)
      glBufferData gl_ARRAY_BUFFER bytes ary gl_STATIC_DRAW
      glEnableVertexAttribArray 0
      glVertexAttribPointer 0 4 gl_FLOAT 0 0 nullPtr
      glDrawArrays gl_TRIANGLES 0 3

    glBindBuffer gl_ARRAY_BUFFER 0

    swapBuffers
    loop buffer
