#version 110

attribute vec4  position;
attribute vec4  faceColor;
uniform   float angle;
varying   vec4  color;

void main() {
  vec3 move = vec3(cos(angle)*0.85, sin(angle)*0.95, 0.0);

  mat4 translate = mat4(
    vec4(    1.0,    0.0,    0.0,    0.0),
    vec4(    0.0,    1.0,    0.0,    0.0),
    vec4(    0.0,    0.0,    1.0,    0.0),
    vec4( move.x, move.y, move.z,    1.0));

  mat4 scale = mat4(
    vec4(    0.5,    0.0,     0.0,    0.0),
    vec4(    0.0,    1.0,     0.0,    0.0),
    vec4(    0.0,    0.0,     1.0,    0.0),
    vec4(    0.0,    0.0,     0.0,    2.0));

  mat4 rotateZ = mat4(
    vec4( cos(angle),    sin(angle),    0.0,    0.0),
    vec4(-sin(angle),    cos(angle),    0.0,    0.0),
    vec4(        0.0,           0.0,    1.0,    0.0),
    vec4(        0.0,           0.0,    0.0,    1.0));

  mat4 rotateX = mat4(
    vec4(    1.0,           0.0,            0.0,    0.0),
    vec4(    0.0,     cos(angle),    sin(angle),    0.0),
    vec4(    0.0,    -sin(angle),    cos(angle),    0.0),
    vec4(    0.0,           0.0,            0.0,    1.0));

  mat4 projection = mat4(
    vec4(    1.0,    0.0,     0.0,    0.0),
    vec4(    0.0,    1.0,     0.0,    0.0),
    vec4(    0.0,    0.0,    -0.1,    0.0),
    vec4(    0.0,    0.0,     0.0,    1.0));

  gl_Position = projection * translate * scale * rotateX * rotateZ * position;
  color       = faceColor;
}
