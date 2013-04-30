#version 110
#define Pi 3.1416

attribute vec4  position;
attribute vec4  translation;
attribute vec4  normal;
attribute vec4  faceColor;

uniform float aspectRatio;
uniform int   specular;
uniform float shininess;
uniform float sunAngle;
uniform float gamma;
uniform float range;

varying vec4 vColor;
varying vec3 vNormal;
varying vec3 vPosition;
varying vec3 vCenter;

void main() {
  mat4 translate = mat4(
    vec4(    1.0,    0.0,    0.0,    0.0),
    vec4(    0.0,    1.0,    0.0,    0.0),
    vec4(    0.0,    0.0,    1.0,    0.0),
    translation);

  float hfov    = Pi/4.0;
  float vfov    = Pi/4.0;
  float x_scale = aspectRatio * (1.0/tan(hfov/2.0));
  float y_scale = 1.0/tan(vfov/2.0);

  mat4 projection = mat4(
    vec4( x_scale,     0.0,     0.0,    0.0),
    vec4(     0.0, y_scale,     0.0,    0.0),
    vec4(     0.0,     0.0,     0.0,    1.0),
    vec4(     0.0,     0.0,    -0.1,    0.0));

  float spinRate = sunAngle * (translation.z/24.0);
  mat4 rotation = mat4(
    vec4(cos(spinRate), -sin(spinRate), 0.0, 0.0),
    vec4(sin(spinRate),  cos(spinRate), 0.0, 0.0),
    vec4(          0.0,            0.0, 1.0, 0.0),
    vec4(          0.0,            0.0, 0.0, 1.0));

  vec4 adjustedPosition = rotation * translate * position;

  gl_Position = projection * adjustedPosition;
  vColor      = faceColor;
  vNormal     = normalize(normal.xyz);
  vPosition   = adjustedPosition.xyz;
  vCenter     = translation.xyz;
}
