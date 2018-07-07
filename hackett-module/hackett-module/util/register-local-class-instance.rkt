#lang racket/base

(provide register-local-class-instance!
         (for-syntax register-local-class-instance!))

(require syntax/parse/define
         "phase1-eval-defer-reconstruct.rkt"
         (for-syntax racket/base
                     racket/match
                     racket/syntax
                     hackett/private/expand+elaborate
                     hackett/private/typeclass))

(begin-for-syntax
  (define (register-local-class-instance! inst)
    (define new-instances (list inst))
    (current-local-class-instances
     (append new-instances (current-local-class-instances)))))

(define-syntax-parser register-local-class-instance!
  [(head inst)
   #'(phase1-eval/defer-reconstruct
      (register-local-class-instance! inst))])

