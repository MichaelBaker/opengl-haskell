#version 110

varying vec2 point;
varying vec2 p0;
varying vec2 p1;
varying vec2 p2;

float distance              (float root);
float cubeRoot              (float a);
float sum                   (vec2 a);
float add                   (vec2 a);
bool withinStroke           (float root, float strokeWidth);
vec2 curve                  (float t);
vec3 solveForThreeRealRoots (float a, float b, float c, float d, float g, float f, float h);
vec3 solveForOneRealRoot    (float a, float b, float c, float d, float g, float f, float h);
vec3 solveForEqualRoots     (float a, float b, float c, float d, float g, float f, float h);
vec3 cubicRoots             (float a, float b, float c, float d);
vec4 grey                   (float c, float root, float width);

void main(void) {
  vec2 alpha = p0 - (2.0 * p1) + p2;
  vec2 beta  = 2.0 * (p1 - p0);
  float a    = add(-2.0 * (alpha * alpha));
  float b    = add(-3.0 * (alpha * beta));
  float c    = add((2.0 * alpha * point) - (2.0 * alpha * p0) - (beta * beta));
  float d    = add(beta * (point - p0));

  vec3 roots = cubicRoots(a, b, c, d);
  float width = 0.05;

  if(withinStroke(roots.x, width))
    gl_FragColor = grey(0.0, roots.x, width);
  else if(withinStroke(roots.y, width))
    gl_FragColor = grey(0.0, roots.y, width);
  else if(withinStroke(roots.z, width))
    gl_FragColor = grey(0.0, roots.z, width);
  else
    discard;
}

vec2 curve(float t) {
  return (pow(1.0-t, 2.0)*p0) + (2.0*(1.0-t)*t*p1) + (pow(t, 2.0)*p2);
}

float distance(float root) {
  return length(point - curve(root));
}

vec4 grey(float c, float root, float width) {
  float alpha = smoothstep(0.0, 0.005, width - distance(root));
  return vec4(c, c, c, alpha);
}

float sum(vec2 a) {
  return a.x + a.y;
}

float cubeRoot(float a) {
  if(a < 0.0)
    return -pow(-a, 1.0/3.0);
  else
    return pow(a, 1.0/3.0);
}

bool withinStroke(float root, float strokeWidth) {
  return root >= 0.0 && root <= 1.0 && distance(root) < strokeWidth;
}

float add(vec2 a) {
  return a.x + a.y;
}

vec3 solveForThreeRealRoots(float a, float b, float c, float d, float g, float f, float h) {
  float i  = sqrt((pow(g, 2.0) / 4.0) - h);
  float j  = cubeRoot(i);
  float k  = acos(-(g / (2.0*i)));
  float l  = j * (-1.0);
  float m  = cos(k / 3.0);
  float n  = sqrt(3.0 * sin(k / 3.0));
  float p  = (b / (3.0*a)) * (-1.0);
  float x1 = (2.0*j) * (cos(k / 3.0)) + p;
  float x2 = l * (m + n) + p;
  float x3 = l * (m - n) + p;
  return vec3(x1, x2, x3);
}

vec3 solveForOneRealRoot(float a, float b, float c, float d, float g, float f, float h) {
  float r = -(g/2.0) + sqrt(h);
  float s = cubeRoot(r);
  float t = -(g/2.0) - sqrt(h);
  float u = cubeRoot(t);
  float x = (s + u) - (b / (3.0*a));
  return vec3(x, -1.0, -1.0);
}

vec3 solveForEqualRoots(float a, float b, float c, float d, float g, float f, float h) {
  float x = cubeRoot((d / a) * (-1.0));
  return vec3(x, -1.0, -1.0);
}

vec3 cubicRoots(float a, float b, float c, float d) {
  float f = ((3.0*a*c) - (b*b)) / (3.0*a*a);
  float g = ((2.0*b*b*b) - (9.0*a*b*c) + (27.0*a*a*d)) / (27.0*a*a*a);
  float h = (pow(g, 2.0) / 4.0) + (pow(f, 3.0) / 27.0);
  if(h < 0.0)
    return solveForThreeRealRoots(a, b, c, d, g, f, h);
  else if(h > 0.0)
    return solveForOneRealRoot(a, b, c, d, g, f, h);
  else
    return solveForEqualRoots(a, b, c, d, g, f, h);
}

