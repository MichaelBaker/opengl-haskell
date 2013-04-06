#!/bin/bash

gcc -o main c-src/main.c \
    -I/usr/local/Cellar/glew/1.9.0/include/GL/ \
    -L/usr/local/Cellar/glew/1.9.0/lib/ \
    -I/opt/local/include \
    -framework OpenGL -framework GLUT -lGLEW
