#lang racket

(require rackunit)
(require "../testing/testing.rkt")

(require "c.rkt")

(module+ test
  (check-output? (display-c-type 'int) "int")
  (check-output? (display-c-type 'char) "char")
  (check-output? (display-c-type '(* int)) "int*")
  (check-output? (display-c-type '(* (* char))) "char**")
  
  (check-output? (display-c-decl '(define (int main (int argc) ((* (* char)) argv)) (break) (break) (break)))
               "int main(int argc, char** argv) {
    break;
    break;
    break;
}

")
  (check-output? (display-c-stmt
                  '(while q
                          (if x
                              (if y
                                  (break)
                                  (while (+ 1 2)
                                         (while i
                                                (do-while
                                                 (break)
                                                 asdf))))
                              (if z
                                  (continue)
                                  (return))))
                  0)
                 "while (q) {
    if (x) {
        if (y) {
            break;
        } else {
            while (1 + 2) {
                while (i) {
                    do {
                        break;
                    } while (asdf);
                }
            }
        }
    } else {
        if (z) {
            continue;
        } else {
            return;
        }
    }
}
")
  (check-output? (display-c-decl '(include "stdlib.h")) "#include \"stdlib.h\"\n")
  (check-output? (display-c-decl '(define (void moo (long-long-int x) ((* (* void)) h) (char c1) (char c2)) (begin (break) (break))))
                 "void moo(long long int x, void** h, char c1, char c2) {
    break;
    break;
}

")
  (check-output? (display-c-stmt '(set! (* hello) (+ 3 (& a))) 0) "*hello = 3 + &a;
"))