#lang racket/base

(provide
 (module-out #%dot))

(require syntax/parse/define
         "../namespace/reqprov.rkt"
         (for-syntax racket/base))


;; a ModulePath is one of:
;;   - Identifier
;;   - #'(#%dot ModulePath Symbol)
;; represents a path to a module or submodule

(define-syntax #%dot
  (syntax-parser
    [(_ mp ~! x:id)
     (raise-syntax-error #f
       "nested module #%dot unimplemented")]))
