#include <stdlib.h>
#include <stdio.h>
#include <GL/glew.h>
#include <GLUT/glut.h>

static int make_resources(void) {
  return 1;
}

static int render(void) {
  glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
  glClear(GL_COLOR_BUFFER_BIT);
  glutSwapBuffers();
}

static void update_fade_factor(void) {
}

int main(int argc, char** argv)
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE);
  glutInitWindowSize(400, 300);
  glutCreateWindow("Hello World");
  glutDisplayFunc(&render);
  glutIdleFunc(&update_fade_factor);
  glewInit();

  if(!GLEW_VERSION_2_0) {
    fprintf(stderr, "OpenGL 2.0 not available\n");
    return 1;
  }

  if(!make_resources()) {
    fprintf(stderr, "Failed to load resources\n");
    return 1;
  }

  glutMainLoop();
  return 0;
}
