(module main ()
(import chicken scheme)
(use extras c-expr)

(map (lambda (f) (display-c-program #f (read-file f))) (command-line-arguments))

)
