#lang racket

(require "../../../pattern-matcher/pattern-matcher.rkt")

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

;; Useful links
;; * C syntax BNF: http://www.cs.man.ac.uk/~pjj/bnf/c_syntax.bnf

(define tabstop 4)
(define (spaces n) (build-string n (lambda (_) #\space)))

(define (inc n) (+ n 1))

;; C related language definitions

(define-language c-decl c-decl?
  ;; unions etc.
  (include `(include ,string?))
  (struct `(struct ,symbol? (,c-type? ,symbol?) ...))
  (definition `(define (,c-type? ,symbol? (,c-type? ,symbol?) ...) ,c-stmt? ...)))

(define-language c-type c-type?
  (pointer `(* ,c-type?))
  (struct `(struct ,symbol?))
  (void `void)
  (char `char)
  (int `int)
  (long-long-int `long-long-int)
  (float `float)
  (double `double))

(define-language c-stmt c-stmt?
  (begin `(begin ,c-stmt? ...))
  (declare `(declare ,c-type? ,symbol?))
  (assign `(set! ,c-lvalue? ,c-expr?))
  (if `(if ,c-expr? ,c-stmt? ,c-stmt?))
  (while `(while ,c-expr? ,c-stmt? ...))
  (do-while `(do-while ,c-stmt? ,c-expr?))
  (return `(return))
  (return-value `(return ,c-expr?))
  (break `(break))
  (continue `(continue))
  (procedure-call `(,symbol? ,c-expr? ...)))

(define-language c-operator c-operator?
  (add `+) (sub `-) (mul `*) (div `/)
  (bit-and `&) (bit-or `\|) (bit-xor `^)
  (and `&&) (or `\|\|)
  (less-than `<) (greater-than `>)
  (less-than-or-equal `<=) (greater-than-or-equal `>=))

(define-language c-expr c-expr?
  (var symbol?)
  (num number?)
  (str string?)
  (op `(,c-operator? ,c-expr? ,c-expr?))
  (ref `(& ,symbol?))
  (deref `(* ,c-expr?))
  (make-struct `(make-struct (struct ,symbol?) (,symbol? ,c-expr?) ...))
  (struct-ref `(struct-ref ,c-lvalue? ,c-expr?))
  (procedure-call `(,symbol? ,c-expr? ...)))

(define-language c-lvalue c-lvalue?
  (var symbol?)
  (deref `(* ,c-lvalue?))
  (array-ref `(array-ref ,symbol? ,c-expr?)))

;; Mangling

(define (quoted-string s)
  (let ((escape (lambda (khar)
                  (case khar
                    ((#\\) (list #\\ #\\))
                    ((#\") (list #\\ #\"))
                    (else (list khar))))))
    (list->string (append (list #\")
                          (flatten (map  escape (string->list s)))
                          (list #\")))))

(define (mangle-symbol sym)
  (let ((escape (lambda (khar)
                  (case khar
                    ((#\-) (string->list "_"))
                    ((#\_) (string->list "_underscore"))
                    ((#\?) (string->list "_question"))
                    (else (list khar)))))
        (escape-keywords (lambda (l)
                           (cond ((pattern? `(#\c #\h #\a #\r) l) (append (list #\s #\y #\m #\_ ) l))
                                 ((pattern? `(#\s #\y #\m ,char? ...) l) (append (list #\s #\y #\m #\_ #\s #\y #\m)
                                                                                 (cdddr l)))
                                 (else l)))))
    (list->string (escape-keywords (flatten (map escape (string->list (symbol->string sym))))))))

;; C language display functions

(define (display-c-symbol sym)
  (display (mangle-symbol sym)))

(define (for-each-between f comma list)
  (if (null? list)
      #t
      (if (null? (cdr list))
          (f (car list))
          (begin (f (car list))
                 (comma)
                 (for-each-between f comma (cdr list))))))

(define (display-c-decl d)
  (match-language c-decl d
    (include => (lambda (filename)
                  (display "#include \"")
                  (display filename)
                  (display "\"")
                  (newline)))
    (struct => (lambda (name fields)
                 (display "struct ") (display-c-symbol name) (display " {") (newline)
                 (for-each (lambda (field)
                             (display (spaces tabstop))
                             (display-c-type (first field)) (display " ")
                             (display-c-symbol (second field)) (display ";") (newline))
                           fields)
                 (display "};") (newline)))
    (definition => (lambda (ret-type name args body)
                     (display-c-type ret-type) (display " ") (display-c-symbol name) (display "(")
                     (for-each-between (lambda (sig)
                                         (display-c-type (first sig))
                                         (display " ")
                                         (display-c-symbol (second sig)))
                                       (lambda ()
                                         (display ", "))
                                       args)
                     (display ") {") (newline)
                     (for-each (lambda (b) (display-c-stmt (first b) 1)) body)
                     (display "}") (newline)
                     (newline)))))

(define (display-c-type t)
  (match-language c-type t
    (void => (lambda () (display "void")))
    (pointer => (lambda (p) (display-c-type p) (display "*")))
    (struct => (lambda (n) (display "struct ") (display-c-symbol n)))
    (char => (lambda () (display "char")))
    (int => (lambda () (display "int")))
    (long-long-int => (lambda () (display "long long int")))
    (float => (lambda () (display "float")))
    (double => (lambda () (display "double")))
    ))

(define (display-c-operator o)
  (match-language c-operator o
    (add => (lambda () (display "+")))
    (sub => (lambda () (display "-")))
    (mul => (lambda () (display "*")))
    (div => (lambda () (display "/")))
    
    (bit-and => (lambda () (display "&")))
    (bit-or => (lambda () (display "|")))
    (bit-xor => (lambda () (display "^")))
    (and => (lambda () (display "&&")))
    (or => (lambda () (display "||")))
    
    (less-than => (lambda () (display "<")))
    (greater-than => (lambda () (display ">")))
    (less-than-or-equal => (lambda () (display "<=")))
    (greater-than-or-equal => (lambda () (display ">=")))
    ))

(define (display-c-expr e)
  (match-language c-expr e
    (var => (lambda (s)
              (display-c-symbol s)))
    (num => (lambda (n)
              (display n)))
    (str => (lambda (s)
              (display (quoted-string s))))
    (op => (lambda (o p q)
             (display "(")
             (display-c-expr p)
             (display " ")
             (display-c-operator o)
             (display " ")
             (display-c-expr q)
             (display ")")))
    (ref => (lambda (e)
                (display "&")
                (display e)))
    (deref => (lambda (e)
                (display "*")
                (display e)))
    (make-struct => (lambda (name fields)
                      (display "(struct ") (display-c-symbol name) (display "){ ")
                      (for-each-between (lambda (field-value)
                                          (display ".") (display-c-symbol (first field-value))
                                          (display " = ") (display-c-expr (second field-value)))
                                        (lambda () (display ", "))
                                        fields)
                      (display " }")))
    (struct-ref => (lambda (lval field)
                     (display-c-lvalue lval) (display ".") (display-c-expr field)))
    (procedure-call => display-procedure-call)))

(define (display-procedure-call f args)
  (display-c-symbol f)
  (display "(")
  (for-each-between (lambda (param)
                      (display-c-expr (first param)))
                    (lambda ()
                      (display ", "))
                    args)
  (display ")"))
  
(define (display-c-lvalue v)
  (match-language c-lvalue v
    (var => (lambda (v)
              (display-c-symbol v)))
    (deref => (lambda (v)
                (display "*")
                (display-c-lvalue v)))
    (array-ref => (lambda (a i)
                    (display-c-symbol a)
                    (display "[")
                    (display-c-expr i)
                    (display "]")))))

(define (display-c-stmt s i)
  (let ((display_ (lambda (s)
                    (display (spaces (* i tabstop)))
                    (display s))))
  (match-language c-stmt s
    (begin => (lambda bs (for-each (lambda (b) (display-c-stmt (first b) i)) (first bs))))
    (declare => (lambda (typ name)
                  (display_ "") (display-c-type typ) (display " ") (display-c-symbol name) (display ";") (newline)))
    (assign => (lambda (lhs rhs)
                 (display_ "")
                 (display-c-lvalue lhs)
                 (display " = ")
                 (display-c-expr rhs)
                 (display ";")
                 (newline)))
    (if => (lambda (c p q)
             (display_ "if (")
             (display-c-expr c)
             (display ") {")
             (newline)
             (display-c-stmt p (inc i))
             (display_ "} else {")
             (newline)
             (display-c-stmt q (inc i))
             (display_ "}")
             (newline)))
    (while => (lambda (c . ss)
                (display_ "while (")
                (display-c-expr c)
                (display ") {")
                (newline)
                (for-each (lambda (s)
                            (display-c-stmt (first s) (inc i)))
                          (first ss))
                (display_ "}")
                (newline)))
    (do-while => (lambda (s c)
                   (display_ "do {")
                   (newline)
                   (display-c-stmt s (inc i))
                   (display_ "} while (")
                   (display-c-expr c)
                   (display ");")
                   (newline)))
    (return => (lambda ()
                 (display_ "return;")
                 (newline)))
    (return-value => (lambda (v)
                       (display_ "return ")
                       (display-c-expr v)
                       (display ";")
                       (newline)))
    (break => (lambda ()
                (display_ "break;")
                (newline)))
    (continue => (lambda ()
                   (display_ "continue;")
                   (newline)))
    (procedure-call => (lambda (f args)
                         (display_ "") (display-procedure-call f args) (display ";")
                         (newline))))))
