#version 110

uniform sampler2D texture;
varying vec2      coordinate;

void main(void) {
  vec2 newCoordinate = coordinate;
  newCoordinate.x += sin(coordinate.y * 24.0 * 3.1415)/50.0;
  gl_FragColor = texture2D(texture, newCoordinate);
}
