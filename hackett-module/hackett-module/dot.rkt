#lang racket/base
(provide
 (rename-out [#%dot_e #%dot])
 (type-out (rename-out [#%dot_τ #%dot]))
 (for-syntax reintroduce-#%dot))

(require
 syntax/parse/define
 (except-in hackett/private/type-language
            ~type
            value-namespace-introduce
            type-namespace-introduce)
 "rep/sig-literals.rkt"
 "namespace/reqprov.rkt"
 (prefix-in l: "link/mod.rkt")
 (for-syntax racket/base
             racket/bool
             racket/match
             racket/syntax
             syntax/parse
             (only-in syntax/parse [attribute @])
             (only-in hackett/private/typecheck attach-type)
             (only-in hackett/private/prop-case-pattern-expander
                      prop:case-pattern-expander)
             hackett/private/util/stx
             "prop-reintroducible-dot-type.rkt"
             "prop-dot-accessible.rkt"
             "check/module-var.rkt"
             "namespace/namespace.rkt"
             "util/stx-traverse.rkt"
             (for-syntax racket/base
                         racket/syntax
                         syntax/parse)))

(begin-for-syntax
  (define disappeared-use 'disappeared-use)

  (struct proc+case-pat-exp [proc case-pat-trans]
    #:property prop:procedure (struct-field-index proc)
    #:property prop:case-pattern-expander
    (λ (self) (proc+case-pat-exp-case-pat-trans self)))
  )

(define-syntax #%dot_e
  (proc+case-pat-exp
   ;; as a normal macro
   (syntax-parser
     #:literal-sets [sig-literals]

     [(_ {~module m:dot-accessible-id/value} ~! x:id)
      #:do [(define key (namespaced:value (syntax-e #'x)))
            (define val-id
              ((@ m.key->id) key))]
      #:fail-unless val-id
      (format "~a is not bound to a value within ~a"
              (syntax-e #'x)
              (syntax->datum #'m))
      (syntax-property
       val-id
       disappeared-use
       (syntax-local-introduce #'m))])

   ;; as a case-pattern expander
   (syntax-parser
     #:literal-sets [sig-literals]
     [(_ {~module m:dot-accessible-id/pattern} ~! x:id)
      #:do [(define key (namespaced:value (syntax-e #'x)))
            (define pat-id
              ((@ m.key->id) key))]
      #:fail-unless pat-id
      (format "~a is not bound to a pattern within ~a"
              (syntax-e #'x)
              (syntax->datum #'m))
      (syntax-property
       pat-id
       disappeared-use
       (syntax-local-introduce #'m))])))

(define-syntax-parser #%dot_τ
  [(_ {~module m:dot-accessible-id/type} ~! x:id)
   #:do [(define key (namespaced:type (syntax-e #'x)))
         (define type-id
           ((@ m.key->id) key))]
   #:fail-unless type-id
   (format "~a is not bound to a type within ~a"
           (syntax-e #'x)
           (syntax->datum #'m))
   (syntax-property
    type-id
    disappeared-use
    (syntax-local-introduce #'m))])

;; -------------------------------------------

(begin-for-syntax
  ;; the ctx contains a module-binding for m-dots-are-from-id
  ;; ASSUME s-to-reintro is already expanded
  (define (reintroduce-#%dot m-dots-are-from-id m-to-prefix-id s-to-reintro ctx)
    (define origin-sym
      (dot-origin-module-sym
       (syntax-local-value m-dots-are-from-id #f ctx)))
    (define/syntax-parse m/prefix
      m-to-prefix-id)

    ;; determine which opaque cons to substitute by comparing
    ;; their mod internal id's with the prefix, to see if we should
    ;; insert prefixes for them.

    (define (traverse stx)
      (syntax-parse stx
        #:literals [#%type:con]
        [{~or (#%type:con {~var x (reintroducible-dot-type-id ctx)})
              {~var x (reintroducible-dot-type-id ctx)}}
         (if (symbol=? (@ x.module-sym) origin-sym)
             (syntax/loc stx (#%dot_τ m/prefix x.external-sym))
             stx)]

        [_
         (traverse-stx/recur stx traverse)]))

    (traverse s-to-reintro)))
