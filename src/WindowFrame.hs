module WindowFrame (createWindowFrame) where

import Graphics.Rendering.OpenGL.Raw
import Foreign

import Job
import Resources
import Renderable
import FrameBuffer
import RenderBuffer
import Texture
import Uniform

data WindowFrame = WindowFrame { texture   :: Uniform GLint
                               , textureId :: GLuint
                               , job       :: Job
                               }

instance Renderable WindowFrame where
  render (WindowFrame texture textureId job) = do
    render texture
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_2D textureId
    render job


createWindowFrame textureId = do
  program        <- createProgram "render"
  position       <- positionAttribute program
  element        <- createElements [0, 1, 2, 2, 3, 0]
  textureUniform <- createUniformInt program "texture" 0
  return $ WindowFrame textureUniform textureId $ Job program position element

positionAttribute program = do
  position       <- attributeId program "position"
  vertexArrayPtr <- newArray verticies
  vertexBuffer   <- createBuffer gl_ARRAY_BUFFER vertexArrayPtr (listSize verticies)
  return [AttributeArray vertexBuffer position 2 gl_FLOAT (fromIntegral $ 2 * floatSize) nullPtr]

verticies = [-1.0, -1.0, 1.0, -1.0, 1.0, 1.0, -1.0, 1.0] :: [GLfloat]
