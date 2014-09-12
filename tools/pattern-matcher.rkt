#lang racket

(provide pattern? match
         language-pattern-names language-patterns define-language match-language)

(require racket/set)
(require srfi/1)

(define (pattern? p e)
  (cond ((null? p) (if (null? e) '() #f))
        ((equal? p '_) (list e))
        ((symbol? p) (if (equal? p e) '() #f))
        ((procedure? p) (if (p e) (list e) #f))
        ((pair? p) (if (pair? e)
                       (let ((lhs (pattern? (car p) (car e))))
                         (if (equal? '(...) (cdr p))
                             (pattern* (car p) (cdr e) (list lhs))
                             (if lhs (let ((rhs (pattern? (cdr p) (cdr e))))
                                       (if rhs (append lhs rhs)
                                           #f)) #f)))
                       #f))
        (else (error "bad pattern"))))

(define (pattern* p e acc)
  (if (null? e)
      (reverse acc)
      (if (pair? e)
          (let ((next (pattern? p (car e))))
            (if next
                (pattern* p (cdr e) (cons next acc))
                #f))
          #f)))

(define (apply-list-to f) (lambda (l) (apply f l)))

(define-syntax match
  (syntax-rules (=> else)
    ((match <exp> (<pattern> => <result>) ... (else <else>))
     (cond ((pattern? <pattern> <exp>) => (apply-list-to <result>)) ... (else <else>)))
    ((match <exp> (<pattern> => <result>) ...)
     (cond ((pattern? <pattern> <exp>) => (apply-list-to <result>)) ...))))

(define (language-pattern-names language) (map car language))
(define (language-patterns language) (map cdr language))
(define-syntax define-language
  (syntax-rules ()
    ((_ <language> <language?> (<name> <pattern>) ...)
     (begin
       (define (<language>) `((<name> ,<pattern>) ...))
       (define (<language?> t)
         (if (any (lambda (pattern) (pattern? pattern t)) `(,<pattern> ...))
             #t #f))))))

(define-syntax match-language
  (syntax-rules (=>)
    ((_ <language> <exp> (<name> => <k>) ...)
     (begin
       (let* ((lang (<language>))
              (missing-clauses (set-subtract (language-pattern-names lang) (list '<name> ...)))
              (extra-clauses (set-subtract (list '<name> ...) (language-pattern-names lang))))
         (if (not (null? missing-clauses))
             (error "Missing clauses in pattern match for language " '<language> ': missing-clauses)
             (when (not (null? extra-clauses))
               (error "Extra clauses in pattern match for language " '<language> ': extra-clauses))))
       (let ((e <exp>))
         (match e
           ((second (assoc '<name> (<language>))) => <k>) ...
           (else (error "Pattern match failed for " '<language> "with" e))))))))
