#lang racket

(require rackunit)
(require rackunit/log)

(provide check-output?)

(define-syntax check-output?
  (syntax-rules ()
    ((check-output? <action> <expected> <message>)
     (check-equal? (with-output-to-string (lambda () <action>)) <expected> <message>))))

(define (hello-world) (display "Hello world!"))

(module+ test
  (check-output? (hello-world) "Hello world!" "test test program"))

