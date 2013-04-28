#version 110

attribute vec2    position;
uniform sampler2D texture;
varying vec2      coordinate;

void main(void) {
  gl_Position = vec4(position, 0.0, 1.0);
  coordinate  = (position + 1.0) / 2.0;
}
