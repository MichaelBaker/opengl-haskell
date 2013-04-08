#version 110

attribute vec4 position;
varying vec4 x;

void main() {
  gl_Position = position;
  x           = position;
}
