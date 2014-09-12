#lang racket

(require "../tools/pattern-matcher.rkt")

(provide c-def c-def?
         c-type c-type?
         c-stmt c-stmt?
         c-operator c-operator?
         c-expr c-expr?
         
         display-c-type)

(define-language c-def c-def?
  ;; unions etc.
  (definition `(define (,c-type? ,symbol? (,c-type? ,symbol?) ...) ,c-stmt?)))

(define-language c-type c-type?
  (pointer `(* ,c-type?))
  (int `int)
  (char `char))

(define-language c-stmt c-stmt?
  (begin `(begin ,c-stmt? ...))
  (if `(if ,c-expr? ,c-stmt? ,c-stmt?))
  (while `(while ,c-expr? ,c-stmt?))
  (do-while `(do-while ,c-stmt? ,c-expr?))
  (return `(return))
  (return-value `(return ,c-expr?))
  (break `(break))
  (continue `(continue)))

(define-language c-operator c-operator?
  (add `+) (sub `-) (mul `*) (div `/))

(define-language c-expr c-expr?
  (var symbol?)
  (num number?)
  (op `(,c-operator ,c-expr ,c-expr))
  (deref `(* ,c-expr)))

(define (display-c-type t)
  (match-language c-type t
    (pointer => (lambda (p) (display-c-type p) (display "*")))
    (int => (lambda () (display "int")))
    (char => (lambda () (display "char")))))
