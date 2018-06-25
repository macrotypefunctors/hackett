#lang racket/base

(provide (module-out seal))

(require syntax/parse/define
         "namespace/reqprov.rkt"
         (for-syntax racket/base
                     syntax/parse
                     "rep/sig.rkt"
                     "check/expand-check.rkt"
                     "namespace/namespace.rkt"))

(define-syntax-parser seal
  #:datum-literals [:>]
  [(_ m:expr :> {~signature s:sig})
   #:with m- (sig⇐ #'m #'s.expansion)
   (attach-sig #'(let-values ([() s.residual])
                   m-)
               #'s.expansion)])

