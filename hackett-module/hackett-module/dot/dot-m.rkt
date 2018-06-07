#lang racket/base

(provide
 (module-out #%dot))

(require syntax/parse/define
         "../namespace/reqprov.rkt"
         (for-syntax racket/base))

;; a ModulePrefix is one of:
;;   - Identifier
;;   - #'(#%dot ModulePrefix Symbol)

(define-syntax #%dot
  (syntax-parser
    [(_ mp ~! x:id)
     (raise-syntax-error #f
       "nested module #%dot unimplemented")]))
