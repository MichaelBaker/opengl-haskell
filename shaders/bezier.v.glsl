#version 110

attribute vec4  position;
attribute float index;

varying vec3 p;
varying vec3 p0;
varying vec3 p1;
varying vec3 p2;

void main(void) {
  gl_Position = position;

  p  = position.xyz;
  p0 = vec3( 1.0, 0.0, 0.0);
  p1 = vec3( 1.0, 1.0, 0.0);
  p2 = vec3( 0.0, 1.0, 0.0);
}
