#version 110

varying vec4 x;

void main() {
  gl_FragColor = vec4( 1.0 - sqrt((x[0] * x[0]) + (x[1] * x[1]))
                     , 0.0
                     , 0.0
                     , 1.0);
}
