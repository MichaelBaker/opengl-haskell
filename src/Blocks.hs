module Blocks where

import Graphics.Rendering.OpenGL.Raw

import Job
import Resources
import Foreign

createBlocks = do
  program    <- createProgram "blocks"
  attributes <- makeBlocksAttributes program
  elements   <- createElements blockElements
  return $ Job program attributes elements

makeBlocksAttributes program = do
  position       <- attributeId program "position"
  faceColor      <- attributeId program "faceColor"
  vertexArrayPtr <- newArray verticies
  vertexBuffer   <- createBuffer gl_ARRAY_BUFFER vertexArrayPtr (listSize verticies)
  let positions    = AttributeArray vertexBuffer position  4 gl_FLOAT (fromIntegral $ 8 * floatSize) nullPtr
      faceColors   = AttributeArray vertexBuffer faceColor 4 gl_FLOAT (fromIntegral $ 8 * floatSize) (createOffset floatSize 4)
  return [positions, faceColors]

verticies :: [GLfloat]
verticies = [-1.0, -1.0, 0.0, 1.0
            , 1.0,  0.0, 0.0, 1.0
            , 0.0, -1.0, 0.0, 1.0
            , 1.0,  0.0, 0.0, 1.0
            , 0.0,  0.0, 0.0, 1.0
            , 1.0,  0.0, 0.0, 1.0

            , 0.0,  0.0, 0.0, 1.0
            , 1.0,  0.0, 0.0, 1.0
            ,-1.0,  0.0, 0.0, 1.0
            , 1.0,  0.0, 0.0, 1.0
            ,-1.0, -1.0, 0.0, 1.0
            , 1.0,  0.0, 0.0, 1.0

            ,-1.0,  1.0, 0.0, 1.0
            , 0.0,  1.0, 0.0, 1.0
            , 0.0,  1.0, 0.0, 1.0
            , 0.0,  1.0, 0.0, 1.0
            , 0.0,  0.0, 0.0, 1.0
            , 0.0,  1.0, 0.0, 1.0

            , 0.0,  0.0, 0.0, 1.0
            , 0.0,  1.0, 0.0, 1.0
            ,-1.0,  0.0, 0.0, 1.0
            , 0.0,  1.0, 0.0, 1.0
            ,-1.0,  1.0, 0.0, 1.0
            , 0.0,  1.0, 0.0, 1.0

            , 0.0,  1.0, 0.0, 1.0
            , 0.0,  0.0, 1.0, 1.0
            , 1.0,  1.0, 0.0, 1.0
            , 0.0,  0.0, 1.0, 1.0
            , 1.0,  0.0, 0.0, 1.0
            , 0.0,  0.0, 1.0, 1.0

            , 1.0,  0.0, 0.0, 1.0
            , 0.0,  0.0, 1.0, 1.0
            , 0.0,  0.0, 0.0, 1.0
            , 0.0,  0.0, 1.0, 1.0
            , 0.0,  1.0, 0.0, 1.0
            , 0.0,  0.0, 1.0, 1.0

            , 0.0,  0.0, 0.0, 1.0
            , 1.0,  1.0, 0.0, 1.0
            , 1.0,  0.0, 0.0, 1.0
            , 1.0,  1.0, 0.0, 1.0
            , 1.0, -1.0, 0.0, 1.0
            , 1.0,  1.0, 0.0, 1.0

            , 1.0, -1.0, 0.0, 1.0
            , 1.0,  1.0, 0.0, 1.0
            , 0.0, -1.0, 0.0, 1.0
            , 1.0,  1.0, 0.0, 1.0
            , 0.0,  0.0, 0.0, 1.0
            , 1.0,  1.0, 0.0, 1.0
            ]

blockElements  :: [GLshort]
blockElements  = [0..23]
