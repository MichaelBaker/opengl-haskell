#version 110

uniform vec3 center;
varying vec3 vPosition;

void main(void) {
  vec3 adjustedPosition = vPosition - center;
  float width    = 0.1;
  float radius   = 1.0 - width;
  float distance = pow(adjustedPosition.x, 2.0) + pow(adjustedPosition.y, 2.0);

  if(distance > radius - width && distance < radius + width)
    gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
  else
    discard;
}
