#lang racket/base

(provide phase1-eval/defer-reconstruct)

(require syntax/parse/define
         (for-syntax racket/base
                     racket/match
                     racket/syntax
                     hackett/private/expand+elaborate
                     hackett/private/typeclass))

(define-syntax-parser phase1-eval/defer-reconstruct
  [(head expr)
   (match (syntax-local-elaborate-pass)
     ['expand
      (syntax-local-eval #'expr)
      (syntax-local-elaborate-defer
       (quasisyntax/loc this-syntax
         (head expr)))]
     ['elaborate
      (syntax-local-eval #'expr)
      (syntax-local-elaborate-defer
       (quasisyntax/loc this-syntax
         (head expr)))]
     ['finalize
      (syntax-local-eval #'expr)
      #'(begin)])])

