#lang racket

(require rackunit)

(provide check-output?)

(define-syntax check-output?
  (syntax-rules ()
    ((check-output? <action> <expected> <message>)
     (check-equal? (with-output-to-string (lambda () <action>)) <expected> <message>))
    ((check-output? <action> <expected>)
     (check-equal? (with-output-to-string (lambda () <action>)) <expected>))))

(define (hello-world) (display "Hello world!"))

(module+ test
  (check-output? (hello-world) "Hello world!" "test test program"))

