#version 110

uniform sampler2D texture;
varying vec2      coordinate;

void main(void) {
  gl_FragColor = texture2D(texture, coordinate);
}
