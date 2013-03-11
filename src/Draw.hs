{-# LANGUAGE FlexibleInstances #-}

module Draw where

import Graphics.Rendering.OpenGL hiding (Face, Line, rotate, Color, color)
import qualified Graphics.Rendering.OpenGL as G

data Face           = Face [(Float, Float, Float)]
                    | TFace [Transformation] Face
data Line           = Line [(Float, Float, Float)]
data Axis           = X | Y | Z
data Coordinates    = Coordinates
data Transformation = Rotation Axis Float
                    | Color Float
                    | Translation Axis Float
                    | Scale Axis Float

rotate axis degree face@(Face _)   = TFace [Rotation axis degree] face
rotate axis degree (TFace ts face) = TFace (Rotation axis degree : ts) face

color c face@(Face _)   = TFace [Color c] face
color c (TFace ts face) = TFace (Color c : ts) face

translate axis amount face@(Face _)   = TFace [Translation axis amount] face
translate axis amount (TFace ts face) = TFace (Translation axis amount : ts) face

scale axis amount face@(Face _)   = TFace [Scale axis amount] face
scale axis amount (TFace ts face) = TFace (Scale axis amount : ts) face

cube :: Float -> [Face]
cube side = [a, b, c, d, e, f]
  where s'    = side / 2.0
        a     = Face [p0, p3, p2, p1]
        b     = Face [p1, p2, p6, p5]
        c     = Face [p5, p6, p7, p4]
        d     = Face [p4, p7, p3, p0]
        e     = Face [p4, p0, p1, p5]
        f     = Face [p7, p3, p2, p6]
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
  draw (TFace ts face)  = preservingMatrix $ do
    mapM_ draw ts
    draw face

instance Draw Line where
  draw (Line verticies) = renderPrimitive LineStrip $ mapM_ draw verticies

instance Draw Transformation where
  draw (Translation axis amount) = G.translate (scaleVector (axisToTuple axis) amount)
  draw (Rotation axis angle)     = G.rotate (vtf angle) (axisToVector axis)
  draw (Color c)                 = G.color $ Color4 (vtf c) (vtf c) (vtf c) (vtf 1)
  draw (Scale X amount)          = G.scale (vtf amount) 1 1
  draw (Scale Y amount)          = G.scale 1 (vtf amount) 1
  draw (Scale Z amount)          = G.scale 1 1 (vtf amount)

instance Draw Coordinates where
  draw Coordinates = do
    renderPrimitive LineStrip $ do
      G.color $ Color4 1 0 0 (vtf 1)
      drawVertex3 (0, 0, 0)
      drawVertex3 (0.5, 0, 0)
    renderPrimitive LineStrip $ do
      G.color $ Color4 0 1 0 (vtf 1)
      drawVertex3 (0, 0, 0)
      drawVertex3 (0, 0.5, 0)
    renderPrimitive LineStrip $ do
      G.color $ Color4 0 0 1 (vtf 1)
      drawVertex3 (0, 0, 0)
      drawVertex3 (0, 0, 1)

axisToVector :: Axis -> Vector3 GLfloat
axisToVector X = vector3 1 0 0
axisToVector Y = vector3 0 1 0
axisToVector Z = vector3 0 0 1

axisToTuple :: Axis -> (Float, Float, Float)
axisToTuple X = (1, 0, 0)
axisToTuple Y = (0, 1, 0)
axisToTuple Z = (0, 0, 1)

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

scaleVector (a, b, c) x = vector3 (a*x) (b*x) (c*x)
