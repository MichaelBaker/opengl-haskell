#version 110
#define Pi 3.1416

attribute vec4  position;
attribute vec4  translation;
attribute vec4  normal;
attribute vec4  faceColor;
uniform   float vfov;
uniform   float hfov;
uniform   float sunAngle;
varying   vec4  color;

void main() {
  mat4 translate = mat4(
    vec4(    1.0,    0.0,    0.0,    0.0),
    vec4(    0.0,    1.0,    0.0,    0.0),
    vec4(    0.0,    0.0,    1.0,    0.0),
    translation);

  float aspect  = 0.5;
  float x_scale = aspect * (1.0/tan(hfov/2.0));
  float y_scale = 1.0/tan(vfov/2.0);

  mat4 projection = mat4(
    vec4( x_scale,     0.0,     0.0,    0.0),
    vec4(     0.0, y_scale,     0.0,    0.0),
    vec4(     0.0,     0.0,     0.0,    1.0),
    vec4(     0.0,     0.0,    -0.1,    0.0));

  vec4 irradiance = vec4(1.0, 1.0, 1.0, 1.0);
  vec3 sun        = normalize(vec3(cos(sunAngle), sin(sunAngle), 0.5*sin(sunAngle)));
  vec4 norm       = normalize(normal);
  vec4 lDiff      = faceColor * irradiance * max(dot(sun, norm.xyz), 0.0);

  gl_Position = projection * translate * position;
  color       = lDiff;
}
