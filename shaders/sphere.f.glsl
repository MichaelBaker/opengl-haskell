#version 110
#define Pi              3.1416
#define E               2.71828
#define NumberOfSamples 10
#define SampleSize      0.00019

uniform float     aspectRatio;
uniform sampler2D gradient;

varying vec2 coordinate;

void main() {
  gl_FragColor = texture2D(gradient, (coordinate + 1.0)/2.0);
}
