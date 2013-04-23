#version 110
#define Pi 3.1416

uniform float sunAngle;

varying vec4 vColor;
varying vec3 vNormal;
varying vec3 vPosition;

vec4 specular(vec3 normal, vec3 halfAngle, vec4 irradiance, float hardness);
vec4 diffuse(vec4 color, vec3 sun, vec3 normal);
vec4 global(vec4 color, float amount);

void main() {
  vec3 sun         = normalize(vec3(4.0 + (4.0 * cos(sunAngle)), 0.0, 13.0 + (4.0 * sin(sunAngle))) - vPosition);
  vec3 view        = normalize(-vPosition);
  vec3 norm        = normalize(vNormal);
  vec3 h           = normalize(view + sun);
  vec4 irradiance  = vec4(1.0, 1.0, 1.0, 1.0);

  vec4 global   = global(vColor, 0.30);
  vec4 diffuse  = diffuse(vColor, sun, norm);
  vec4 specular = specular(norm, h, irradiance, 400.0);

  gl_FragColor = global + diffuse + specular;
}

vec4 specular(vec3 normal, vec3 halfAngle, vec4 irradiance, float hardness) {
  float reflection = max(dot(normal, halfAngle), 0.0);
  float intensity  = pow(reflection, hardness);
  return irradiance * intensity;
}

vec4 diffuse(vec4 color, vec3 sun, vec3 normal) {
  return color * max(dot(sun, normal), 0.0);
}

vec4 global(vec4 color, float amount) {
  return color * amount;
}
