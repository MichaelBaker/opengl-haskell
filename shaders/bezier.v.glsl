#version 110

uniform vec2 c0;
uniform vec2 c1;
uniform vec2 c2;

attribute vec4  position;
attribute float index;

varying vec2 point;
varying vec2 p0;
varying vec2 p1;
varying vec2 p2;

void main(void) {
  gl_Position = position;

  point = position.xy;
  p0    = c0;
  p1    = c1;
  p2    = c2;
}
