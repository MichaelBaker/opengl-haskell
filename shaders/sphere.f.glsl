#version 110
#define Pi 3.1416
#define E  2.71828

uniform float aspectRatio;

varying vec4 vColor;

void main() {
  gl_FragColor = vColor;
}
