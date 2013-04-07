#include <stdlib.h>
#include <stdio.h>
#include <GL/glew.h>
#include <GLUT/glut.h>

static struct {
  GLuint vertex_buffer
       , element_buffer
       , vertex_shader
       , fragment_shader
       , program;

  struct {
    GLint position;
  } attributes;

} g_resources;

static const GLfloat g_vertex_buffer_data[] = { 
  -1.0f, -1.0f,
   1.0f, -1.0f,
  -1.0f,  1.0f,
};

static const GLushort g_element_buffer_data[] = { 0, 1, 2 };

static GLuint make_buffer( GLenum     target,
                           const void *buffer_data,
                           GLsizei    buffer_size ) {
  GLuint buffer;
  glGenBuffers(1, &buffer);
  glBindBuffer(target, buffer);
  glBufferData(target, buffer_size, buffer_data, GL_STATIC_DRAW);
  return buffer;
}

static void update_fade_factor(void) {
}

void *file_contents(const char *filename, GLint *length) {
  FILE *f = fopen(filename, "r");
  void *buffer;

  if (!f) {
    fprintf(stderr, "Unable to open %s for reading\n", filename);
    return NULL;
  }

  fseek(f, 0, SEEK_END);
  *length = ftell(f);
  fseek(f, 0, SEEK_SET);

  buffer = malloc(*length+1);
  *length = fread(buffer, 1, *length, f);
  fclose(f);
  ((char*)buffer)[*length] = '\0';

  return buffer;
}

static void show_info_log( GLuint                    object,
                           PFNGLGETSHADERIVPROC      glGet__iv,
                           PFNGLGETSHADERINFOLOGPROC glGet__InfoLog ) {
  GLint log_length;
  char *log;

  glGet__iv(object, GL_INFO_LOG_LENGTH, &log_length);
  log = malloc(log_length);
  glGet__InfoLog(object, log_length, NULL, log);
  fprintf(stderr, "%s", log);
  free(log);
}

static GLuint make_shader(GLenum type, const char *filename)
{
  GLint length;
  GLchar *source = file_contents(filename, &length);
  GLuint shader;
  GLint shader_ok;

  if (!source)
    return 0;

  shader = glCreateShader(type);
  glShaderSource(shader, 1, (const GLchar**)&source, &length);
  free(source);
  glCompileShader(shader);
  glGetShaderiv(shader, GL_COMPILE_STATUS, &shader_ok);

  if (!shader_ok) {
    fprintf(stderr, "Failed to compile %s:\n", filename);
    show_info_log(shader, glGetShaderiv, glGetShaderInfoLog);
    glDeleteShader(shader);
    return 0;
  }
  return shader;
}

static GLuint make_program(GLuint vertex_shader, GLuint fragment_shader) {
  GLint program_ok;
  GLuint program = glCreateProgram();
  glAttachShader(program, vertex_shader);
  glAttachShader(program, fragment_shader);
  glLinkProgram(program);
  glGetProgramiv(program, GL_LINK_STATUS, &program_ok);
  if (!program_ok) {
    fprintf(stderr, "Failed to link shader program:\n");
    show_info_log(program, glGetProgramiv, glGetProgramInfoLog);
    glDeleteProgram(program);
    return 0;
  }
  return program;
}

static int make_resources(void) {
  g_resources.vertex_buffer = make_buffer(
      GL_ARRAY_BUFFER,
      g_vertex_buffer_data,
      sizeof(g_vertex_buffer_data));

  g_resources.element_buffer = make_buffer(
      GL_ELEMENT_ARRAY_BUFFER,
      g_element_buffer_data,
      sizeof(g_element_buffer_data));

  g_resources.vertex_shader = make_shader(
      GL_VERTEX_SHADER,
      "gl.v.glsl");

  if (g_resources.vertex_shader == 0)
    return 0;

  g_resources.fragment_shader = make_shader(
      GL_FRAGMENT_SHADER,
      "gl.f.glsl");

  if (g_resources.fragment_shader == 0)
    return 0;

  g_resources.program = make_program(
      g_resources.vertex_shader,
      g_resources.fragment_shader);

  if (g_resources.program == 0)
    return 0;

  g_resources.attributes.position
    = glGetAttribLocation(g_resources.program, "position");

  return 1;
}

static void render(void) {
  glUseProgram(g_resources.program);
  glBindBuffer(GL_ARRAY_BUFFER, g_resources.vertex_buffer);
  glVertexAttribPointer(
      g_resources.attributes.position,  /* attribute */
      2,                                /* size */
      GL_FLOAT,                         /* type */
      GL_FALSE,                         /* normalized? */
      sizeof(GLfloat)*2,                /* stride */
      (void*)0                          /* array buffer offset */);
  glEnableVertexAttribArray(g_resources.attributes.position);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, g_resources.element_buffer);
  glDrawElements(
      GL_TRIANGLES,       /* mode */
      3,                  /* count */
      GL_UNSIGNED_SHORT,  /* type */
      (void*)0            /* element array buffer offset */);
  glDisableVertexAttribArray(g_resources.attributes.position);
  glutSwapBuffers();
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
