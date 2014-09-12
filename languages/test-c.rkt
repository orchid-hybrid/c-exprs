#lang racket

(require rackunit)
(require "../testing/testing.rkt")

(require "c.rkt")

(module+ test
  (check-output? (display-c-type 'int) "int")
  (check-output? (display-c-type 'char) "char")
  (check-output? (display-c-type '(* int)) "int*")
  (check-output? (display-c-type '(* (* char))) "char**")
  
  (check-output? (display-c-decl '(define (int main (int argv) ((* (* char)) argv)) (break) (break) (break)))
               "int main(...) {
    break;
    break;
    break;
}

"))
