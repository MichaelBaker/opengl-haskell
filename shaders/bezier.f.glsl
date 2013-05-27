#version 110

varying vec2 point;
varying vec2 p0;
varying vec2 p1;
varying vec2 p2;
varying vec2 av;
varying vec2 bv;
varying vec2 cv;
varying vec2 dv;

float cubeRoot(float a) {
  float sign = 1.0;

  if(a < 0.0) {
    sign = -1.0;
    a    = -a;
  }

  float exponent = ceil(log2(a));
  float mantissa = a / exp2(exponent);
  float shx      = mod(exponent, 3.0);

  if(shx > 0.0) shx -= 3.0;

  exponent = (exponent - shx) / 3.0;
  mantissa = mantissa * exp2(shx);

  mantissa = (((( 45.2548339756803022511987494   * mantissa +
                 192.2798368355061050458134625)  * mantissa +
                 119.1654824285581628956914143)  * mantissa +
                  13.43250139086239872172837314) * mantissa +
                   0.1636161226585754240958355063)
             /
             (((( 14.80884093219134573786480845 * mantissa +
                 151.9714051044435648658557668) * mantissa +
                 168.5254414101568283957668343) * mantissa +
                  33.9905941350215598754191872) * mantissa +
                   1.0);

  return mantissa * sign * exp2(exponent);
}

vec2 curve(float t) {
  return (pow(1.0-t, 2.0)*p0) + (2.0*(1.0-t)*t*p1) + (pow(t, 2.0)*p2);
}

float distance(vec2 a, vec2 b) {
  return length(a - b);
}

vec4 grey(float c) {
  return vec4(c, c, c, 1.0);
}

bool withinStroke(float root, float strokeWidth) {
  return root >= 0.0 && root <= 1.0 && distance(point, curve(root)) < strokeWidth;
}

float add(vec2 a) {
  return a.x + a.y;
}

float solveForThreeRealRoots(float a, float b, float c, float d, float g, float f, float h) {
  float i  = sqrt((pow(g, 2.0) / 4.0) - h);
  float j  = cubeRoot(i);
  float k  = acos(-(g / (2.0*i)));
  float l  = j * (-1.0);
  float m  = cos(k / 3.0);
  float n  = sqrt(3.0 * sin(k / 3.0));
  float p  = (b / (3.0*a)) * (-1.0);

  float x1   = (2.0*j) * (cos(k / 3.0)) + p;
  float x2   = l * (m + n) + p;
  float x3   = l * (m - n) + p;
  float temp = x1;

  if(distance(point, curve(x1)) < distance(point, curve(x2)) && distance(point, curve(x1)) < distance(point, curve(x3)))
    return x1;
  else if(distance(point, curve(x2)) < distance(point, curve(x3)))
    return x2;
  else
    return x3;
}

float solveForOneRealRoot(float a, float b, float c, float d, float g, float f, float h) {
  float r = -(g/2.0) + sqrt(h);
  float s = cubeRoot(r);
  float t = -(g/2.0) - sqrt(h);
  float u = cubeRoot(t);
  return (s + u) - (b / (3.0*a));
}

float solveForEqualRoots(float a, float b, float c, float d, float g, float f, float h) {
  return cubeRoot((d / a) * (-1.0));
}

float cubicRoot(float a, float b, float c, float d) {
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

void main(void) {
  float width = 0.05;
  float a     = add(av);
  float b     = add(bv);
  float c     = add(cv);
  float d     = add(dv);

  float root = cubicRoot(a, b, c, d);

  if(withinStroke(root, width))
    gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
  else
    discard;
}
