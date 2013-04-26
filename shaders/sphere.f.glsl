#version 110
#define Pi 3.1416
#define E  2.71828

uniform int   specular;
uniform float shininess;
uniform float sunAngle;
uniform float gamma;
uniform float range;

varying vec4 vColor;
varying vec3 vNormal;
varying vec3 vPosition;

vec4 blinnPhong        (vec3 normal, vec3 halfAngle, vec3 sun, vec4 irradiance, float hardness);
vec4 gaussian          (vec3 normal, vec3 halfAngle, vec3 sun, vec4 irradiance, float hardness);
vec4 diffuse           (vec4 color, vec3 sun, vec3 normal);
vec4 global            (vec4 color, float amount);
vec4 gammaCorrection   (vec4 color, float power);
vec4 colorCorrection   (vec4 color, float gamma, float range);
vec3 sunVector         (float sunAngle, vec3 surfacePosition);
vec4 specularHighlight (int type, vec3 normal, vec3 halfAngle, vec3 sunVector, vec4 irradiance, float shininess);

void main() {
  vec3 sun         = sunVector(sunAngle, vPosition);
  vec3 view        = normalize(-vPosition);
  vec3 normal      = normalize(vNormal);
  vec3 halfAngle   = normalize(view + sun);
  vec4 irradiance  = vec4(1.0, 1.0, 1.0, 1.0);

  vec4 globalLight   = global(vColor, 0.05);
  vec4 diffuseLight  = diffuse(vColor, sun, normal);
  vec4 specularLight = specularHighlight(specular, normal, halfAngle, sun, irradiance, shininess);

  gl_FragColor = colorCorrection(globalLight + diffuseLight + specularLight, gamma, range);
}

vec4 specularHighlight(int type, vec3 normal, vec3 halfAngle, vec3 sunVector, vec4 irradiance, float shininess) {
  vec4 highlight = vec4(0.0, 0.0, 0.0, 0.0);

  if(type == 1)
    highlight = blinnPhong(normal, halfAngle, sunVector, irradiance, shininess*100.0);
  else if(type == 2)
    highlight = gaussian(normal, halfAngle, sunVector, irradiance, shininess);

  return min(highlight * 7.0, 1.0) * 0.95;
}

vec3 sunVector(float sunAngle, vec3 surfacePosition) {
  return normalize(vec3(8.0 * cos(sunAngle), 0.0, 13.0 + (4.0 * sin(sunAngle*2.0))) - surfacePosition);
}

vec4 colorCorrection(vec4 color, float gamma, float range) {
  return gammaCorrection(color, gamma) / range;
}

vec4 gammaCorrection(vec4 color, float power) {
  float r = pow(color.r, power);
  float g = pow(color.g, power);
  float b = pow(color.b, power);
  return vec4(r, g, b, color.a);
}

vec4 blinnPhong(vec3 normal, vec3 halfAngle, vec3 sun, vec4 irradiance, float hardness) {
  float reflection = max(dot(normal, halfAngle), 0.0);
  float intensity  = pow(reflection, hardness);
  vec4  exitance   = irradiance * dot(sun, normal);
  return exitance * intensity;
}

vec4 gaussian(vec3 normal, vec3 halfAngle, vec3 sun, vec4 irradiance, float hardness) {
  float reflection = dot(normal, halfAngle);
  float power      = pow(acos(reflection)/hardness, 2.0);
  float intensity  = max(pow(E, -power), 0.0);
  vec4  exitance   = irradiance * dot(sun, normal);
  return exitance * intensity;
}

vec4 diffuse(vec4 color, vec3 sun, vec3 normal) {
  return color * max(dot(sun, normal), 0.0);
}

vec4 global(vec4 color, float amount) {
  return color * amount;
}
