#version 110
#define Pi 3.1416

attribute vec4  position;
attribute vec4  faceColor;

varying vec4 vColor;
varying vec3 vPosition;

void main() {
  gl_Position = position;
  vColor      = faceColor;
  vPosition   = position.xyz;
}
