#version 110
#define Pi 3.1416

attribute vec4  position;
attribute vec4  translation;
attribute vec4  normal;
attribute vec4  faceColor;

uniform   float vfov;
uniform   float hfov;
uniform   float sunAngle;
uniform   float aspectRatio;

varying vec4 vColor;
varying vec4 vNormal;
varying vec4 vPosition;

void main() {
  mat4 translate = mat4(
    vec4(    1.0,    0.0,    0.0,    0.0),
    vec4(    0.0,    1.0,    0.0,    0.0),
    vec4(    0.0,    0.0,    1.0,    0.0),
    translation);

  float hfov    = Pi/4.0;
  float vfov    = Pi/4.0;
  float x_scale = aspectRatio * (1.0/tan(hfov/2.0));
  float y_scale = 1.0/tan(vfov/2.0);

  mat4 projection = mat4(
    vec4( x_scale,     0.0,     0.0,    0.0),
    vec4(     0.0, y_scale,     0.0,    0.0),
    vec4(     0.0,     0.0,     0.0,    1.0),
    vec4(     0.0,     0.0,    -0.1,    0.0));

  mat4 rotate = mat4(
      vec4( cos(sunAngle), sin(sunAngle), 0.0, 0.0),
      vec4(-sin(sunAngle), cos(sunAngle), 0.0, 0.0),
      vec4(           0.0,           0.0, 1.0, 0.0),
      vec4(           0.0,           0.0, 0.0, 1.0));

  gl_Position = projection * rotate * translate * position;
  vColor      = faceColor;
  vNormal     = normal;
  vPosition   = gl_Position;
}
