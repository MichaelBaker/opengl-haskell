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
varying vec2 av;
varying vec2 bv;
varying vec2 cv;
varying vec2 dv;

void main(void) {
  gl_Position = position;

  point = position.xy;
  p0    = c0;
  p1    = c1;
  p2    = c2;

  vec2 alpha = p0 - (2.0 * p1) + p2;
  vec2 beta  = 2.0 * (p1 - p0);

  av    = -2.0 * (alpha * alpha);
  bv    = -3.0 * (alpha * beta);
  cv    = (2.0 * alpha * point) - (2.0 * alpha * p0) - (beta * beta);
  dv    = beta * (point - p0);
}
