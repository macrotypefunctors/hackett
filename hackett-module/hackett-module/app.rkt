#lang racket/base

(provide (type-out (rename-out [type-#%app #%app])))

(require syntax/parse/define
         "namespace/reqprov.rkt"
         "rep/apply-type.rkt"
         (prefix-in hkt-type: (unmangle-in #:no-introduce #:only hackett/base))
         (for-syntax racket/base
                     syntax/parse/class/paren-shape))

(define-syntax-parser type-#%app
  [(~parens _ t . args)
   (syntax/loc this-syntax (#%apply-type t . args))]
  [{~braces _ . stuff}
   ;; don't copy the properties over, trust that the macro
   ;; expander will
   (datum->syntax this-syntax
                  (cons #'hkt-type:#%app #'stuff)
                  this-syntax)])

