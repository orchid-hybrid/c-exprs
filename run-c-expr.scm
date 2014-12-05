(use extras)
(include "c-expr.scm")

(module main ()
  (import chicken scheme extras)
  (import c-expr)

(map (lambda (f) (display-c-program #t (read-file f))) (command-line-arguments))

)
