#lang racket/base

(provide (type-out (rename-out [type-#%app #%app]))
         (module-out (rename-out [appₘ #%app])))

(require syntax/parse/define
         "namespace/reqprov.rkt"
         "rep/apply-type.rkt"
         "rep/sig-literals.rkt"
         (prefix-in hkt-type: (unmangle-in #:no-introduce #:only hackett/base))
         (for-syntax racket/base
                     syntax/parse/class/paren-shape
                     "check/expand-check.rkt"
                     "rep/sig.rkt"))

(define-syntax-parser type-#%app
  [(~parens _ t . args)
   (syntax/loc this-syntax (#%apply-type t . args))]
  [{~braces _ . stuff}
   ;; don't copy the properties over, trust that the macro
   ;; expander will
   (datum->syntax this-syntax
                  (cons #'hkt-type:#%app #'stuff)
                  this-syntax)])

(define-syntax-parser appₘ
  #:literals [#%pi-sig]
  [(_ fun:expr arg:id)
   ;; TODO: allow module paths for `a`, or module expressions if possible
   #:with [fun- (#%pi-sig ([x A]) B)] (sig⇒ #'fun)
   #:with arg- (sig⇐ #'arg #'A)
   #:with B*:sig (signature-subst #'B #'x #'arg)
   (attach-sig #'(#%app fun- arg-) #'B*.expansion)])

