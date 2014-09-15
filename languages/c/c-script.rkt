#lang racket

(require racket/cmdline)
(require racket/file)

(require "c.rkt")

;; The accompanying script lets you turn 'c-expr scripts' into C files
;;
;; sh to-c.sh scripts/hello-world.rkt

(let ((args (current-command-line-arguments)))
  (if (= 1 (vector-length args))
      (let* ((filename (vector-ref args 0))
             (contents (file->list filename)))
        (for-each display-c-decl contents))
      (error "pass a single filename to the command only")))
