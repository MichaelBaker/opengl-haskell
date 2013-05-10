#version 110

uniform   vec3 center;
attribute vec4 position;
varying   vec3 vPosition;

void main(void) {
  gl_Position = position;
  vPosition   = position.xyz;
}
