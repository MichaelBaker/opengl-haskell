{-# LANGUAGE FlexibleInstances #-}

module Draw where

import Graphics.Rendering.OpenGL hiding (Face, Line, rotate, Color)
import qualified Graphics.Rendering.OpenGL as G

data Face       = Face [(Float, Float, Float)]
data Line       = Line [(Float, Float, Float)]
data Rotation a = Rotation Axis Float a
data Axis       = X | Y | Z
data Color a    = Color Float a

rotate = Rotation

cube :: Float -> [Face]
cube side = [a, b, c, d, e, f]
  where s'    = side / 2.0
        a     = Face [p0, p1, p2, p3]
        b     = Face [p1, p5, p6, p2]
        c     = Face [p5, p4, p7, p6]
        d     = Face [p4, p0, p3, p7]
        e     = Face [p4, p5, p1, p0]
        f     = Face [p7, p6, p2, p3]
        p0    = (-s',  s',  s')
        p1    = ( s',  s',  s')
        p2    = ( s',  s', -s')
        p3    = (-s',  s', -s')
        p4    = (-s', -s',  s')
        p5    = ( s', -s',  s')
        p6    = ( s', -s', -s')
        p7    = (-s', -s', -s')

class Draw a where
  draw :: a -> IO ()

instance (Draw a) => Draw [a] where
  draw items = mapM_ draw items

instance Draw (Float, Float) where
  draw = drawVertex

instance Draw (Float, Float, Float) where
  draw = drawVertex3

instance Draw Face where
  draw (Face verticies) = renderPrimitive Polygon $ mapM_ draw verticies

instance Draw Line where
  draw (Line verticies) = renderPrimitive LineStrip $ mapM_ draw verticies

instance (Draw a) => Draw (Rotation a) where
  draw (Rotation axis angle a) = do
    preservingMatrix $ do
      G.rotate (vtf angle) (axisToVector axis)
      draw a

instance (Draw a) => Draw (Color a) where
  draw (Color c a) = do
    color $ Color4 (vtf c) (vtf c) (vtf c) (vtf 1)
    draw a

axisToVector :: Axis -> Vector3 GLfloat
axisToVector X = vector3 1 0 0
axisToVector Y = vector3 0 1 0
axisToVector Z = vector3 0 0 1

vtf :: Float -> GLfloat
vtf = realToFrac

drawVertex :: (Float, Float) -> IO ()
drawVertex (x, y) = vertex $ vertex3 x y 0.0

drawVertex3 :: (Float, Float, Float) -> IO ()
drawVertex3 (x, y, z) = vertex $ vertex3 x y z

vertex3 :: Float -> Float -> Float -> Vertex3 GLfloat
vertex3 x y z = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

vector3 :: Float -> Float -> Float -> Vector3 GLfloat
vector3 x y z = Vector3 (realToFrac x) (realToFrac y) (realToFrac z)
