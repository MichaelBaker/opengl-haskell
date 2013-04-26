#version 110
#define Pi 3.1416
#define E  2.71828

uniform int   specular;
uniform float shininess;
uniform float sunAngle;
uniform float gamma;

varying vec4 vColor;
varying vec3 vNormal;
varying vec3 vPosition;

vec4 blinnPhong(vec3 normal, vec3 halfAngle, vec4 irradiance, float hardness);
vec4 gaussian(vec3 normal, vec3 halfAngle, vec4 irradiance, float hardness);
vec4 diffuse(vec4 color, vec3 sun, vec3 normal);
vec4 global(vec4 color, float amount);
vec4 gammaCorrection(vec4 color, float power);

void main() {
  vec3 sun         = normalize(vec3(8.0 * cos(sunAngle), 0.0, 13.0 + (4.0 * sin(sunAngle*2.0))) - vPosition);
  vec3 view        = normalize(-vPosition);
  vec3 norm        = normalize(vNormal);
  vec3 h           = normalize(view + sun);
  vec4 irradiance  = vec4(1.0, 1.0, 1.0, 1.0);

  vec4 global            = global(vColor, 0.30);
  vec4 diffuse           = diffuse(vColor, sun, norm);
  vec4 specularHighlight = vec4(0.0, 0.0, 0.0, 0.0);

  if(specular == 1)
    specularHighlight = blinnPhong(norm, h, irradiance, shininess*100.0);
  else if(specular == 2)
    specularHighlight = gaussian(norm, h, irradiance, shininess);

  gl_FragColor = gammaCorrection(global + diffuse + specularHighlight, gamma);
}

vec4 gammaCorrection(vec4 color, float power) {
  float r = pow(color.r, power);
  float g = pow(color.g, power);
  float b = pow(color.b, power);
  return vec4(r, g, b, color.a);
}

vec4 blinnPhong(vec3 normal, vec3 halfAngle, vec4 irradiance, float hardness) {
  float reflection = max(dot(normal, halfAngle), 0.0);
  float intensity  = pow(reflection, hardness);
  return irradiance * intensity;
}

vec4 gaussian(vec3 normal, vec3 halfAngle, vec4 irradiance, float hardness) {
  float reflection = dot(normal, halfAngle);
  float power      = pow(acos(reflection)/hardness, 2.0);
  float intensity  = max(pow(E, -power), 0.0);
  return irradiance * intensity;
}

vec4 diffuse(vec4 color, vec3 sun, vec3 normal) {
  return color * max(dot(sun, normal), 0.0);
}

vec4 global(vec4 color, float amount) {
  return color * amount;
}
