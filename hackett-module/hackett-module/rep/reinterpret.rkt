#lang racket/base

(provide #%apply-type
         (for-syntax reinterpret
                     u-type-literals
                     ?#%apply-type
                     path->u-type-path
                     path->u-mod-path))

(require
 syntax/parse/define
 "../namespace/reqprov.rkt"
 "../rep/sig-literals.rkt"
 (except-in hackett/private/type-alias type)
 (except-in hackett/private/type-language
            value-namespace-introduce type-namespace-introduce ~type)
 (rename-in (unmangle-in "../dot/dot-m.rkt") [#%dot #%dot_m])
 (rename-in (unmangle-in "../dot/dot-t.rkt") [#%dot #%dot_τ])
 (only-in (unmangle-in #:only hackett/base) #%app)
 (for-syntax racket/base
             racket/pretty
             (only-in syntax/parse [attribute @])
             syntax/parse/experimental/template
             syntax/parse/class/local-value
             syntax/apply-transformer
             hackett/private/util/stx
             "../namespace/namespace.rkt"
             "../util/stx-traverse.rkt"
             "../util/disappeared-use.rkt"))


(begin-for-syntax

  (define-syntax-class applicable-type
    #:attributes [apply]
    [pattern {~var head (local-value alias-transformer?)}
      #:attr apply
      (λ (args)
        (local-apply-transformer (@ head.local-value)
                                 #`(head #,@args)
                                 'expression))]

    [pattern (#%dot_τ {~module m:dot-accessible-path/type} ~! x:id)
      #:do [(define key (namespaced:type (syntax-e #'x)))
            (define type-id ((@ m.key->id) key))]
      #:fail-unless type-id
      (format "~a is not bound to a type within ~a"
              (syntax-e #'x)
              (syntax->datum #'m))

      #:with a:applicable-type type-id
      #:attr apply (compose (λ (x) (add-disappeared-use x #'m.root))
                            (@ a.apply))]

    [pattern head
      #:attr apply
      (λ (args)
        (define/syntax-parse [arg ...] args)
        (template (?#%type:app* head arg ...)))])

  )

(define-syntax-parser #%apply-type
  #:literals [#%dot_τ]
  [(_ head) #'head]
  [(_ head:applicable-type arg ...+)
   ((@ head.apply) (@ arg))])

;; A UType ("uninterpreted" type) is one of:
;;   - UTypeApp
;;   - UTypePath
;;   - HackettType (sub parts are UTypes instead of types)
;;     ....

;; A UTypePath is one of:
;;   - Id
;;   - (#%type:app* (#%type:con #%dot_τ) UModulePath (#%type:con Symbol))

;; A UModulePath is one of:
;;   - Id
;;   - (#%type:app* (#%type:con #%dot_m) UModulePath (#%type:con Symbol))

;; A UTypeApp is a:
;;   (#%type:app* (#%type:con #%apply-type)
;;                UTypePath
;;                UType
;;                ...)

(begin-for-syntax

  (define-literal-set u-type-literals
    #:literal-sets [type-literals]
    [#%dot_τ #%dot_m #%apply-type])

  (define-template-metafunction ?#%apply-type
    (syntax-parser
      [(_ a b ...)
       (quasitemplate/loc/props this-syntax
         (?#%type:app* (#%type:con #%apply-type) a b ...))]))

  (define-syntax-class (u-type intdef-ctx)
    #:attributes [norm]
    #:literal-sets [u-type-literals]
    [pattern {~var || (u-type-app intdef-ctx)}]
    [pattern {~var || (u-type-path intdef-ctx)}]
    [pattern stx #:with norm (traverse-stx/recur #'stx reinterpret)])

  (define-syntax-class (u-type-path intdef-ctx)
    #:attributes [norm]
    #:literal-sets [u-type-literals]
    [pattern (~#%type:app* (#%type:con dot:#%dot_τ ~!) {~var mp (u-module-path intdef-ctx)} (#%type:con x:id))
             #:with stx this-syntax
             #:with norm (datum->syntax #'stx (list #'dot #'mp.norm #'x) #'stx)]
    [pattern x:id
             ;; TODO: local-value here?
             #:with norm #'x])

  (define-syntax-class (u-module-path intdef-ctx)
    #:attributes [norm]
    #:literal-sets [u-type-literals]
    [pattern (~#%type:app* (#%type:con dot:#%dot_m ~!) {~var mp (u-module-path intdef-ctx)} (#%type:con x:id))
             #:with stx this-syntax
             #:with norm (datum->syntax #'stx (list #'dot #'mp.norm #'x) #'stx)]
    [pattern x:id
             #:when (syntax-local-value #'x (λ () #f) intdef-ctx)
             #:with norm #'x])

  (define-syntax-class (u-type-app intdef-ctx)
    #:attributes [norm]
    #:literal-sets [u-type-literals]
    [pattern (~#%type:app* (#%type:con app:#%apply-type ~!)
                           {~var tp (u-type-path intdef-ctx)}
                           {~var arg (u-type intdef-ctx)}
                           ...)
             #:with stx this-syntax
             #:with norm
             (datum->syntax #'stx
                            (list* #'app #'tp.norm #'[arg.norm ...])
                            #'stx)])

  ;; Reinterpret the given uninterpreted type, so that the resulting type
  ;; can be expanded into a valid Hackett type.
  ;; UType [IntDefCtx] -> TypeStx
  (define (reinterpret-u-type ut [intdef-ctx #f])
    (syntax-parse ut
      [{~var u (u-type intdef-ctx)} #'u.norm]))

  ;; Stx [IntDefCtx] -> Stx
  ;; where UTypes inside are reinterpreted
  (define (reinterpret stx [intdef-ctx #f])
    (let reinterpret ([stx stx])
      (syntax-parse stx
        [{~var u (u-type intdef-ctx)} #'u.norm]
        [_ (traverse-stx/recur stx reinterpret)])))


  (define (path->u-type-path p)
    (syntax-parse p
      #:literals [#%dot_τ]
      [x:id #'x]
      [(dot:#%dot_τ mp x)
       #:with mp* (path->u-mod-path #'mp)
       (template/loc p
        (?#%type:app* (#%type:con dot)
                      mp*
                      (#%type:con x)))]))

  (define (path->u-mod-path p)
    (syntax-parse p
      #:literals [#%dot_m]
      [x:id #'x]
      [(dot:#%dot_m mp x)
       #:with mp* (path->u-mod-path #'mp)
       (template/loc p
        (?#%type:app* (#%type:con dot)
                      mp*
                      (#%type:con x)))]))

  )

(module+ test
  (define-syntax M- #t)

  (begin-for-syntax
    (require rackunit)
    (check-equal?
     (syntax->datum
      (reinterpret
       (template
        (?#%type:app* (#%type:con Tuple)
                      (?#%type:app* (#%type:con #%dot_τ)
                                    M-
                                    (#%type:con T))
                      (?#%type:app* (#%type:con #%apply-type)
                                    (?#%type:app*
                                     (#%type:con #%dot_τ)
                                     (?#%type:app* (#%type:con #%dot_m)
                                                   M-
                                                   (#%type:con N))
                                     (#%type:con A))
                                    (#%type:con Integer))))))

     (syntax->datum
      (template
       (?#%type:app* (#%type:con Tuple)
                     (#%dot_τ M- T)
                     (#%apply-type (#%dot_τ (#%dot_m M- N) A)
                                   (#%type:con Integer))))))

    ))

  #;(
  (sig (module M : (sig (type T)))
       (type (O a))
       (type (U a) = {M.T -> a}))

  SIG EXPANSION:
  intdef-ctx
  : M- value (#f)
  : M = internal: M-
        prop:dot-acc/type
          T => T1
  : T1 = var-transformer (app* (con #%dot) M- (con T))

  : O- value (#f)
  : O = alias-transformer (a) (app* (con #%apply-type) O- a)

  : U- value (#f)
  : U = alias-transformer (a) (app* (con #%apply-type) U- a)

  ==>
  (#%sig (['M M-]
          )
         (['M (#%module-decl
               (#%sig [T T-] [T #%opaque]))]))


  DEF-MODULE BINDING:
  (def-module K <the-sig>)
  1. reinterpret signature
  2. bind things in type-expansion-context
  3. expand types with t-e-c

  U: (#%alias [a] {(#%dot M T) -> a})

  (define M1 (l:mod ...))
  (define-syntax M
    (....
     internal-id: M1
     prop:dot-acc/type
       T => T5))

  (define-syntax T5 (opaque-type-transformer 'M99 'T))

  (define K1 (l:mod ....))
  (define-syntax K
    (....
     unique symbol: 'K89
     internal-id: K1
     prop:dot-acc/type
       O => O2
       U => U3
       ))

  (define-syntax O2
    (opaque-type-transformer 'K89 'O))

  )
