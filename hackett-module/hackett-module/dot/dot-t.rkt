#lang racket/base

(provide (type-out #%dot)
         (for-syntax reintroduce-#%dot))

(require syntax/parse/define
         "../namespace/reqprov.rkt"
         "../rep/sig-literals.rkt"
         (only-in "dot-m.rkt" dot-accessible-path/type)
         (only-in hackett/private/type-language #%type:con)
         (for-syntax racket/base
                     racket/bool
                     "../namespace/namespace.rkt"
                     "../prop-dot-accessible.rkt"
                     "../prop-reintroducible-dot-type.rkt"
                     "../util/stx-traverse.rkt"
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

(begin-for-syntax

  ;; Id ModulePrefix Signature IntDefCtx -> SignatureStx
  ;; ASSUME s-to-reintro is already expanded
  ;; - `m-dots-are-from-id` should be bound (in ctx or surrounding env) to
  ;;   a `prop:dot-origin` struct which designates which references to reintroduce
  ;;   #%dot for.
  ;; - `m-to-prefix-id` is used as the prefix for the #%dot form that gets
  ;;   reintroduced for every occurence in `s-to-reintro`.
  (define (reintroduce-#%dot m-dots-are-from-id m-prefix s-to-reintro ctx)
    (define origin-sym
      (dot-origin-module-sym
       (syntax-local-value m-dots-are-from-id #f ctx)))

    (define/syntax-parse m-prefix*
      m-prefix)

    ;; determine which opaque cons to substitute by comparing
    ;; their mod internal id's with the prefix, to see if we should
    ;; insert prefixes for them.

    (define (traverse stx)
      (syntax-parse stx
        #:literals [#%type:con]
        [{~or (#%type:con {~var x (reintroducible-dot-type-id ctx)})
              {~var x (reintroducible-dot-type-id ctx)}}
         (if (symbol=? (@ x.module-sym) origin-sym)
             (syntax/loc stx (#%dot m-prefix* x.external-sym))
             stx)]

        [_ (traverse-stx/recur stx traverse)]))

    (traverse s-to-reintro))
  )
