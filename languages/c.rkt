#lang racket

(require "../tools/pattern-matcher.rkt")

(provide c-def c-def?
         c-type c-type?
         c-stmt c-stmt?
         c-operator c-operator?
         c-expr c-expr?
         
         display-c-type)

(define tabstop 4)

(define (inc n) (+ n 1))

(define (spaces n)
  (build-string n (lambda (_) #\space)))

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
  (op `(,c-operator? ,c-expr? ,c-expr?))
  (deref `(* ,c-expr?)))

(define (display-c-type t)
  (match-language c-type t
    (pointer => (lambda (p) (display-c-type p) (display "*")))
    (int => (lambda () (display "int")))
    (char => (lambda () (display "char")))))

(define (display-c-expr e)
  (match-language c-expr e
    (var => (lambda (s)
              (display s)))
    (num => (lambda (n)
              (display n)))
    (op => (lambda (o p q)
             (display p)
             (display " ")
             (display o)
             (display " ")
             (display q)))
    (deref => (lambda (e)
                (display "*")
                (display e)))))

(define (display-c-stmt s i)
  (let ((display_ (lambda (s)
                    (display (spaces (* i tabstop)))
                    (display s))))
  (match-language c-stmt s
    (begin => (lambda bs (for-each (lambda (b) (display-c-stmt (first b) i) (newline)) bs)))
    (if => (lambda (c p q)
             (display_ "if (")
             (display-c-expr c)
             (display ") {")
             (newline)
             (display-c-stmt p (inc i))
             (newline)
             (display_ "} else {")
             (newline)
             (display-c-stmt q (inc i))
             (newline)
             (display_ "}")))
    (while => (lambda (c s)
                (display_ "while (")
                (display-c-expr c)
                (display ") {")
                (newline)
                (display-c-stmt s (inc i))
                (newline)
                (display_ "}")))
    (do-while => (lambda (s c)
                   (display_ "do {")
                   (newline)
                   (display-c-stmt s (inc i))
                   (newline)
                   (display_ "} while (")
                   (display-c-expr c)
                   (display ");")))
    (return => (lambda ()
                 (display_ "return;")))
    (return-value => (lambda (v)
                       (display_ "return (")
                       (display-c-expr v)
                       (display ");")))
    (break => (lambda ()
                (display_ "break;")))
    (continue => (lambda ()
                   (display_ "continue;"))))))