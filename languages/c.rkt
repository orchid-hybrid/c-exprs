#lang racket

(require "../tools/pattern-matcher.rkt")

(define-language c-def c-def?
  (definition `(define (,c-type? ,symbol? (,c-type? ,symbol?) ...) ,c-stmt?)))

(define-language c-type c-type?
  (pointer `(* ,c-type?))
  (int `int)
  (char `char))

(define-language lambda-calculus lambda-calculus?
  (int number?)
  (var symbol?)
  (lambda `(lambda (_) _))
  (app `(_ _)))

(match-language lambda-calculus exp
  (int => (lambda () 1))
  (var => (lambda () 1))
  (lambda => (lambda (param body) 1))
  (app => (lambda (fn arg) 1)))

