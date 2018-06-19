#lang racket/base

(require "../namespace/reqprov.rkt"
         "../rep/sig-literals.rkt"
         "../rep/apply-type.rkt"
         (only-in (unmangle-in #:no-introduce hackett/base) Integer ∀ ->)
         (only-in (unmangle-in hackett/base) Integer ∀ -> #%app)
         (only-in hackett/private/type-language
                  ~#%type:app*
                  ~#%type:forall*
                  ~->*)

         (unmangle-in #:no-introduce "../sig.rkt")
         (rename-in (unmangle-in "../dot/dot-t.rkt") [#%dot #%dot_τ])
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
                    (#%val-decl (#%apply-type X1-ref:id))])
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
                    (#%val-decl (#%apply-type X1-ref:id))])
                  #:when (free-identifier=? #'X1 #'X1-ref))

  (check-stxparse (expand-sig
                   #'(sig
                      (type Y)
                      (type X = Y)
                      (val x : X)))
                  #:literal-sets [sig-literals u-type-literals]
                  (~sig
                   [#s(namespaced type Y)
                    Y1
                    (#%type-decl (#%opaque []))]
                   [#s(namespaced type X)
                    X2
                    (#%type-decl (#%alias [] (#%apply-type Y1-ref:id)))]
                   [#s(namespaced value x)
                    x3
                    (#%val-decl (#%apply-type X2-ref:id))])
                  #:when (free-identifier=? #'Y1 #'Y1-ref)
                  #:when (free-identifier=? #'X2 #'X2-ref))

  (check-stxparse (expand-sig
                   #'(sig
                      (type Y)
                      (type X = Y)
                      (val x : Y)))
                  #:literal-sets [sig-literals u-type-literals]
                  (~sig
                   [#s(namespaced type Y)
                    Y1
                    (#%type-decl (#%opaque []))]
                   [#s(namespaced type X)
                    X2
                    (#%type-decl (#%alias [] (#%apply-type Y1-ref1:id)))]
                   [#s(namespaced value x)
                    x3
                    (#%val-decl (#%apply-type Y1-ref2:id))])
                  #:when (free-identifier=? #'Y1 #'Y1-ref1)
                  #:when (free-identifier=? #'Y1 #'Y1-ref2))

  ;; ---------------

  (check-stxparse (expand-sig
                   (sig (type (X a))
                        (val x : (X Integer))))
                  #:literal-sets [sig-literals u-type-literals]
                  #:literals [Integer]
                  (~sig
                   [#s(namespaced type X)
                    X1
                    (#%type-decl (#%opaque [a1]))]
                   [#s(namespaced value x)
                    x2
                    (#%val-decl (#%apply-type X1-ref:id (#%type:con Integer)))])
                  #:when (free-identifier=? #'X1 #'X1-ref))

  ;; ---------------

  (check-stxparse (sig (type (C A))
                       (val make : (∀ [A] {A -> (C A)})))
                  #:literal-sets [sig-literals u-type-literals]
                  #:literals [Integer]
                  (~sig
                   [#s(namespaced type C) C1 (#%type-decl (#%opaque _))]
                   [#s(namespaced value make)
                    mk1
                    (#%val-decl
                     {~#%type:forall* [A]
                       {~->* A-ref1
                             (#%apply-type C-ref:id A-ref2:id)}})])
                  #:when (free-identifier=? #'C1 #'C-ref)
                  #:when (free-identifier=? #'A #'A-ref1)
                  #:when (free-identifier=? #'A #'A-ref2))

  (check-stxparse (sig (type (C A) = A)
                       (val make : (∀ [A] {A -> (C A)})))
                  #:literal-sets [sig-literals u-type-literals]
                  #:literals [Integer]
                  (~sig
                   [#s(namespaced type C) C1 (#%type-decl (#%alias _ _))]
                   [#s(namespaced value make)
                    mk1
                    (#%val-decl
                     {~#%type:forall* [A]
                       {~->* A-ref1:id
                             (#%apply-type C-ref:id A-ref2:id)}})])
                  #:when (free-identifier=? #'C1 #'C-ref)
                  #:when (free-identifier=? #'A #'A-ref1)
                  #:when (free-identifier=? #'A #'A-ref2))

  ;; ---------------

  (check-stxparse (expand-sig
                   (sig (module M : (sig (type X)))
                        (val x : (#%dot_τ M X))))
                  #:literal-sets [sig-literals u-type-literals]
                  (~sig
                   [#s(namespaced module M)
                    M1
                    (#%module-decl {~sig
                                    [#s(namespaced type X)
                                     X2
                                     (#%type-decl (#%opaque []))]})]
                   [#s(namespaced value x)
                    x3
                    (#%val-decl (#%apply-type (#%dot_τ M1-ref:id {~datum X})))])
                  #:when (free-identifier=? #'M1 #'M1-ref))

  )
