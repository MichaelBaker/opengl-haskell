#version 110
#define Pi 3.1416

uniform float sunAngle;

varying vec4 vColor;
varying vec4 vNormal;
varying vec4 vPosition;

void main() {
  vec3 sun  = normalize(vec3(sin(sunAngle*10.0), cos(sunAngle*10.0), sin(sunAngle)));
  vec3 view = -normalize(vPosition.xyz);

  // Diffuse lighting
  vec4 irradiance = vec4(1.0, 1.0, 1.0, 1.0);
  vec4 norm       = normalize(vNormal);
  vec4 diffuse    = vColor * irradiance * max(dot(sun, norm.xyz), 0.0);

  // Specular lighting
  vec3 h          = (view + sun)/length(view + sun);
  float intensity = dot(vNormal.xyz, h)/(length(vNormal.xyz)*length(h));
  vec4 specular   = 100.5 * vec4(1.0, 0.0, 0.0, 1.0) * intensity;

  gl_FragColor = (diffuse + specular);
}
