#lang racket

(require "../tools/pattern-matcher.rkt")

(provide c-decl c-decl?
         c-type c-type?
         c-stmt c-stmt?
         c-operator c-operator?
         c-expr c-expr?
         
         display-c-decl
         display-c-type
         display-c-stmt
         display-c-operator
         display-c-expr)

(define tabstop 4)
(define (spaces n) (build-string n (lambda (_) #\space)))

(define (inc n) (+ n 1))

;; C related language definitions

(define-language c-decl c-decl?
  ;; unions etc.
  (definition `(define (,c-type? ,symbol? (,c-type? ,symbol?) ...) ,c-stmt? ...)))

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

;; C language display functions

(define (display-c-decl d)
  (match-language c-decl d
    (definition => (lambda (ret-type name args body)
                     (display-c-type ret-type) (display " ") (display name) (display "(")
                     (display "...")
                     (display ") {") (newline)
                     (for-each (lambda (b) (display-c-stmt (first b) 1)) body)
                     (display "}") (newline)
                     (newline)))))

(define (display-c-type t)
  (match-language c-type t
    (pointer => (lambda (p) (display-c-type p) (display "*")))
    (int => (lambda () (display "int")))
    (char => (lambda () (display "char")))))

(define (display-c-operator o)
  (match-language c-operator o
    (add => (lambda () (display "+")))
    (sub => (lambda () (display "-")))
    (mul => (lambda () (display "*")))
    (div => (lambda () (display "/")))))

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
             (display_ "}")
             (newline)))
    (while => (lambda (c s)
                (display_ "while (")
                (display-c-expr c)
                (display ") {")
                (newline)
                (display-c-stmt s (inc i))
                (display_ "}")
                (newline)))
    (do-while => (lambda (s c)
                   (display_ "do {")
                   (newline)
                   (display-c-stmt s (inc i))
                   (newline)
                   (display_ "} while (")
                   (display-c-expr c)
                   (display ");")
                   (newline)))
    (return => (lambda ()
                 (display_ "return;")))
    (return-value => (lambda (v)
                       (display_ "return (")
                       (display-c-expr v)
                       (display ");")
                       (newline)))
    (break => (lambda ()
                (display_ "break;")
                (newline)))
    (continue => (lambda ()
                   (display_ "continue;")
                   (newline))))))
