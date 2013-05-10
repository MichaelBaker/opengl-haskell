#version 110
#define CUBE 1.0/3.0

varying vec3 p;
varying vec3 p0;
varying vec3 p1;
varying vec3 p2;

float sum(vec3 v) {
  return v.x + v.y+ v.z;
}

vec3 curve(float t) {
  return (pow(1.0-t, 2.0)*p0) + ((2.0*(1.0-t)*t)*p1) + (t*t*p2);
}

void main(void) {
  vec3 av = (4.0*p1*p1) - (2.0*p1*p0) - (4.0*p2*p1) + (2.0*p0*p2) - (4.0*p0*p1) + (2.0*p0*p0) + (4.0*p1*p1) - (2.0*p0*p1);
  vec3 bv = (-4.0*p1*p1) + (4.0*p1*p0) + (4.0*p2*p1) - (4.0*p2*p0) + (4.0*p0*p1) - (4.0*p0*p0) - (4.0*p1*p1) + (4.0*p0*p1) + (4.0*p0*p1) - (2.0*p0*p0) + (2.0*p1*p0);
  vec3 cv = (-2.0*p1*p0) + (2.0*p2*p0) + (2.0*p0*p0) - (2.0*p0*p1) - (4.0*p0*p1) + (4.0*p0*p0) + (4.0*p1*p1) - (4.0*p1*p0) - (2.0*p*p1) + (2.0*p*p2) + (2.0*p*p0) - (2.0*p*p1);
  vec3 dv = (-2.0*p0*p0) + (2.0*p1*p0) - (2.0*p*p0) + (2.0*p*p1);

  float a = sum(av);
  float b = sum(bv);
  float c = sum(cv);
  float d = sum(dv);

  float s = ((3.0*a*c) - (b*b)) / (3.0*a*a);
  float q = ((2.0*b*b*b) - (9.0*a*b*c) + (27.0*a*a*d)) / (27.0*a*a*a);

  float q2   = (-q) / 2.0;
  float qs4  = (q*q) / 4.0;
  float pc27 = (s*s*s) / 27.0;

  float x = sqrt(qs4 + pc27);

  float y1= q2 + x;
  float z1= q2 - x;

  float y2= q2 + x;
  float z2= q2 + x;

  float y3= q2 - x;
  float z3= q2 - x;

  float u1 = pow(y1, (1.0/3.0));
  float v1 = pow(z1, (1.0/3.0));

  float u2 = pow(y2, (1.0/3.0));
  float v2 = pow(z2, (1.0/3.0));

  float u3 = pow(y3, (1.0/3.0));
  float v3 = pow(z3, (1.0/3.0));

  float t1 = u1 + v1;
  float t2 = u2 + v2;
  float t3 = u3 + v3;

  if(t1 >= 0.0 && t1 <= 1.0 && length(p - curve(t1)) < 0.1)
    gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
  else if(t2 >= 0.0 && t2 <= 1.0 && length(p - curve(t2)) < 0.1)
    gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
  else if(t3 >= 0.0 && t3 <= 1.0 && length(p - curve(t3)) < 0.1)
    gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
  else
    discard;
}
