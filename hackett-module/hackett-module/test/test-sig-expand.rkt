#lang racket/base

(require "../namespace/reqprov.rkt"
         "../rep/sig-literals.rkt"
         "../rep/reinterpret.rkt"
         (unmangle-in #:no-introduce "../sig.rkt")
         (for-syntax racket/base
                     syntax/parse
                     syntax/parse/define
                     "../private/test/check-stxparse.rkt"
                     "../rep/sig.rkt"
                     "../util/stx.rkt"
                     (for-syntax racket/base
                                 syntax/parse)))

(begin-for-syntax
  (define-simple-macro (sig stuff |...|) (expand-sig #`(sig stuff |...|)))
  (define-simple-macro (pi stuff |...|) (expand-sig #`(Π stuff |...|)))

  (define-syntax ~sig
    (pattern-expander
     (syntax-parser
       [(_ [key-datum internal-id-pat decl-pat] ...)
        #'({~literal #%sig}
           {~hash [{~datum key-datum} internal-id-pat] ...}
           {~hash [{~datum key-datum} decl-pat] ...})])))

  (error-print-width 1000)

  (check-stxparse (sig (type X)
                       (val x : X))
                  #:literal-sets [sig-literals u-type-literals]
                  (~sig
                   [#s(namespaced type X)
                    X1
                    (#%type-decl (#%opaque []))]
                   [#s(namespaced value x)
                    x2
                    (#%val-decl (#%type:app (#%type:con #%apply-type)
                                            X1-ref))])
                  #:when (free-identifier=? #'X1 #'X1-ref))

  (check-stxparse (expand-sig
                   (sig (type X)
                        (val x : X)))
                  #:literal-sets [sig-literals u-type-literals]
                  (~sig
                   [#s(namespaced type X)
                    X1
                    (#%type-decl (#%opaque []))]
                   [#s(namespaced value x)
                    x2
                    (#%val-decl (#%type:app (#%type:con #%apply-type)
                                            X1-ref:id))])
                  #:when (free-identifier=? #'X1 #'X1-ref))

  )
