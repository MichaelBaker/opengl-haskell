#version 110

attribute vec2 position;
varying vec4 x;

void main() {
  gl_Position = vec4(position, 0.0, 1.0); // Screen position

  x = vec4( position[0]
          , position[0]
          , position[0]
          , 1.0);
}
