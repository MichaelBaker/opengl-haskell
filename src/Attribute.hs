module Attribute ( createAttributes
                 , enableAttribute
                 , disableAttribute
                 , fourFloatVector
                 , singleFloat
                 , AttributeDescription(..)
                 , Attribute()) where

import Foreign
import Foreign.C.String
import Graphics.Rendering.OpenGL.Raw
import Utilities

type AttributeId = GLuint

data AttributeDescription = AttributeDescription { attributeName   :: String
                                                 , components      :: GLint
                                                 , openGLType      :: GLenum
                                                 , shouldNormalize :: GLenum
                                                 } deriving (Show)

data Attribute = Attribute { attributeId :: GLuint
                           , description :: AttributeDescription
                           , stride      :: GLint
                           , offset      :: Int
                           , bufferId    :: GLuint
                           } deriving (Show)

fourFloatVector = AttributeDescription "fourFloatVector" 4 gl_FLOAT gl_FALSE
singleFloat     = AttributeDescription "singleFloat"     1 gl_FLOAT gl_FALSE

createAttributes :: GLuint -> [GLfloat] -> [AttributeDescription] -> IO [Attribute]
createAttributes program dataPoints descriptions = do
  withArray dataPoints $ \dataPointPointer -> do
    buffer <- createBuffer gl_ARRAY_BUFFER dataPointPointer $ listSize dataPoints
    let stride                  = width descriptions
        descriptionsWithOffsets = zip descriptions $ offsets descriptions
        construct               = uncurry $ compileAttribute program buffer stride
    mapM construct descriptionsWithOffsets

compileAttribute program bufferId stride description offset = do
  attributeId <- getAttributeId program (attributeName description)
  return $ Attribute attributeId description stride offset bufferId

enableAttribute :: Attribute -> IO ()
enableAttribute attribute = do
  glEnableVertexAttribArray (attributeId attribute)
  glBindBuffer gl_ARRAY_BUFFER $ bufferId attribute
  glVertexAttribPointer
    (attributeId attribute)
    (components $ description attribute)
    (openGLType $ description attribute)
    (fromIntegral $ shouldNormalize $ description attribute)
    (stride attribute)
    (plusPtr nullPtr $ offset attribute)

disableAttribute :: Attribute -> IO ()
disableAttribute = glDisableVertexAttribArray . attributeId

width = sum . (map totalSize)

totalSize description = glSizeOf (components description) (openGLType description)

offsets descriptions = 0 : aggregate
  where aggregate = map totalSize $ init descriptions

getAttributeId program name = do
  attribute <- withCString name $ \str -> glGetAttribLocation program str
  return $ fromIntegral attribute

createOffset typeSize amount = plusPtr nullPtr $ typeSize * amount
