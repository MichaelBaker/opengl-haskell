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

  float hfov    = Pi/4.0;
  float vfov    = Pi/4.0;
  float aspect  = 0.5;
  float x_scale = aspect * (1.0/tan(hfov/2.0));
  float y_scale = 1.0/tan(vfov/2.0);

  mat4 projection = mat4(
    vec4( x_scale,     0.0,     0.0,    0.0),
    vec4(     0.0, y_scale,     0.0,    0.0),
    vec4(     0.0,     0.0,     0.0,    1.0),
    vec4(     0.0,     0.0,    -0.1,    0.0));

  vec3 sun  = normalize(vec3(cos(sunAngle), sin(sunAngle), Pi*sin(sunAngle)));
  vec3 view = -normalize(position.xyz);

  // Diffuse lighting
  vec4 irradiance = vec4(1.0, 1.0, 1.0, 1.0);
  vec4 norm       = normalize(normal);
  vec4 diffuse    = faceColor * irradiance * max(dot(sun, norm.xyz), 0.0);

  // Specular lighting
  vec3 h          = (view + sun)/length(view + sun);
  float intensity = dot(normal.xyz, h)/(length(normal.xyz)*length(h));
  vec4 specular   = 2.0 * irradiance * intensity;

  gl_Position = projection * translate * position;
  color       = -(diffuse + specular);
}
