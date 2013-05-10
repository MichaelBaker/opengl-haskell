module Element where

import Foreign
import Graphics.Rendering.OpenGL.Raw
import Utilities

data ElementArray = ElementArray { elementBuffer :: GLuint
                                 , triangleType  :: GLenum
                                 , vertexCount   :: GLsizei
                                 , indexType     :: GLenum
                                 } deriving (Show)

createElements :: [GLshort] -> IO ElementArray
createElements items = do
  withArray items $ \elementArrayPointer -> do
    elementBuffer <- createBuffer gl_ELEMENT_ARRAY_BUFFER elementArrayPointer (listSize items)
    return $ ElementArray elementBuffer gl_TRIANGLES (fromIntegral $ length items) gl_UNSIGNED_SHORT

drawElements elementArray = do
  glBindBuffer gl_ELEMENT_ARRAY_BUFFER (elementBuffer elementArray)
  glDrawElements
    (triangleType elementArray)
    (vertexCount  elementArray)
    (indexType    elementArray)
    nullPtr
