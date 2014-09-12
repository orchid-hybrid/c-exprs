#lang racket

(require rackunit)
(require "../testing/testing.rkt")

(require "pattern-matcher.rkt")

(module+ test
  ;; test null
  (check-equal? (pattern? '() '()) '())
  (check-equal? (pattern? '() 'e) #f)
  
  ;; test _
  (check-equal? (pattern? '_ 'foo) '(foo))
  (check-equal? (pattern? '_ 'bar) '(bar))
  
  ;; test symbol?
  (check-equal? (pattern? 'e 'e) '())
  (check-equal? (pattern? 'e 'f) #f)
  
  ;; test procedure?
  (check-equal? (pattern? boolean? #t) '(#t))
  (check-equal? (pattern? boolean? 3) #f)
  
  ;; complex expressions
  (check-equal? (pattern? '(_ _ _) '(a b c)) '(a b c))
  (check-equal? (pattern? '(_ b _) '(a b c)) '(a c))
  (check-equal? (pattern? '(lambda (_) _) '(lambda (x) (f x))) '(x (f x)))
  (check-equal? (pattern? '(lambda (_) _) '(lambda (y) y y y)) #f)
  
  ;; ...
  (check-equal? (pattern? '(_ ...) '(a)) '(((a))))
  (check-equal? (pattern? '(_ ...) '(a b)) '(((a) (b))))
  (check-equal? (pattern? '(_ ...) '(a b c)) '(((a) (b) (c))))
  (check-equal? (pattern? `(,symbol? ...) '(a b c)) '(((a) (b) (c))))
  (check-equal? (pattern? `(,symbol? ...) '(a b 3 c)) #f)
  (check-equal? (pattern? `(yeah ((ok ,symbol?) ...)) '(yeah ((ok kid) (ok kid) (ok dude) (ok buddy))))
                '(((kid) (kid) (dude) (buddy))))
  )


(define-language foo foo?
  (a 'a)
  (b 'b))

(define (bar x) (match-language foo x
                  (a => (lambda () (display 'a)))
                  (b => (lambda () (display 'b)))))
