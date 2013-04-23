#version 110

attribute vec4  position;
attribute vec4  faceColor;
uniform   float angle;
varying   vec4  color;

void main() {
  mat4 translate = mat4(
    vec4(1.0, 0.0, 0.0, 0.0),
    vec4(0.0, 1.0, 0.0, 0.0),
    vec4(0.0, 0.0, 1.0, 0.0),
    vec4(0.0, 0.0, 0.0, 1.0));

  mat4 rotateZ = mat4(
    vec4( cos(angle), sin(angle), 0.0, 0.0),
    vec4(-sin(angle), cos(angle), 0.0, 0.0),
    vec4(        0.0,        0.0, 1.0, 0.0),
    vec4(        0.0,        0.0, 0.0, 1.0));

  mat4 rotateX = mat4(
    vec4(1.0,        0.0,         0.0, 0.0),
    vec4(0.0, cos(angle), -sin(angle), 0.0),
    vec4(0.0, sin(angle),  cos(angle), 0.0),
    vec4(0.0,        0.0,         0.0, 1.0));

  gl_Position = rotateX * rotateZ * position;
  color       = faceColor;
}
