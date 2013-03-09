{-# LANGUAGE FlexibleInstances #-}

module Draw where

import Graphics.Rendering.OpenGL

class Draw a where
  draw :: a -> IO ()

instance (Draw a) => Draw [a] where
  draw items = renderPrimitive LineStrip $ mapM_ draw items

instance Draw (Float, Float) where
  draw = drawVertex

instance Draw (Float, Float, Float) where
  draw = drawVertex3

drawVertex :: (Float, Float) -> IO ()
drawVertex (x, y) = vertex $ vertex3 x y 0.0

drawVertex3 :: (Float, Float, Float) -> IO ()
drawVertex3 (x, y, z) = vertex $ vertex3 x y z

vertex3 :: Float -> Float -> Float -> Vertex3 GLfloat
vertex3 x y z = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)
