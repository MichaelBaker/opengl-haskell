#version 110
#define PI 3.1415926535897932384626433832795

attribute vec4 position;
attribute vec4 faceColor;
varying   vec4 color;

void main() {
  mat4 translate = mat4(
    vec4(1.0, 0.0, 0.0, 0.0),
    vec4(0.0, 1.0, 0.0, 0.0),
    vec4(0.0, 0.0, 1.0, 0.0),
    vec4(0.0, 0.0, 0.0, 1.0));

  float angleZ = PI/4.0;
  mat4 rotateZ = mat4(
    vec4( cos(angleZ), sin(angleZ), 0.0, 0.0),
    vec4(-sin(angleZ), cos(angleZ), 0.0, 0.0),
    vec4(         0.0,         0.0, 1.0, 0.0),
    vec4(         0.0,         0.0, 0.0, 1.0));

  float angleX = PI/4.0;
  mat4 rotateX = mat4(
    vec4(1.0,         0.0,          0.0, 0.0),
    vec4(0.0, cos(angleX), -sin(angleX), 0.0),
    vec4(0.0, sin(angleX),  cos(angleX), 0.0),
    vec4(0.0,         0.0,          0.0, 1.0));

  gl_Position = rotateX * rotateZ * position;
  color       = faceColor;
}
