#!/bin/sh
racket c-script.rkt $1 > $1.c && gcc $1.c -o $1.out && echo "Build successful!"


