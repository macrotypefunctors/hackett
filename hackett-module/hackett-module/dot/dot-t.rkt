#lang racket/base

(provide (type-out #%dot))

(require syntax/parse/define
         "../namespace/reqprov.rkt"
         "../rep/sig-literals.rkt"
         (only-in "dot-m.rkt" dot-accessible-path/type)
         (for-syntax racket/base
                     "../namespace/namespace.rkt"
                     "../util/disappeared-use.rkt"
                     (only-in syntax/parse [attribute @])))

(define-syntax-parser #%dot
  [(_ {~module m:dot-accessible-path/type} ~! x:id)
   #:do [(define key (namespaced:type (syntax-e #'x)))
         (define type-id
           ((@ m.key->id) key))]
   #:fail-unless type-id
   (format "~a is not bound to a type within ~a"
           (syntax-e #'x)
           (syntax->datum #'m))
   (add-disappeared-use
    type-id
    #'m.root)])

