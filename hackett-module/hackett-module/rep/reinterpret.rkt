#lang racket/base

(provide #%apply-type
         (for-syntax reinterpret
                     u-type-literals
                     path->u-type-path
                     path->u-mod-path))

(require
 syntax/parse/define
 "../namespace/reqprov.rkt"
 "../rep/sig-literals.rkt"
 (except-in hackett/private/type-language
            value-namespace-introduce type-namespace-introduce ~type)
 (rename-in (unmangle-in "../dot/dot-m.rkt") [#%dot #%dot_m])
 (rename-in (unmangle-in "../dot/dot-t.rkt") [#%dot #%dot_τ])
 (for-syntax racket/base
             racket/pretty
             (only-in syntax/parse [attribute @])
             syntax/parse/experimental/template
             "../namespace/namespace.rkt"
             "../util/stx-traverse.rkt"
             "../util/disappeared-use.rkt"))

(define-syntax-parser #%apply-type
  #:literals [#%dot_τ]

  [(_ head) #'head]

  [(_ head:id arg ...+)
   #'(head arg ...)]

  [(_ (#%dot_τ {~module m:dot-accessible-path/type} ~! x:id) arg ...+)
   #:do [(define key (namespaced:type (syntax-e #'x)))
         (define type-id ((@ m.key->id) key))]
   #:fail-unless type-id
   (format "~a is not bound to a type within ~a"
           (syntax-e #'x)
           (syntax->datum #'m))
   #:with head type-id
   (add-disappeared-use
    #'(head arg ...)
    #'m.root)])

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

  (define-syntax-class u-type
    #:attributes [norm]
    #:literal-sets [u-type-literals]
    [pattern :u-type-app]
    [pattern :u-type-path]
    [pattern stx #:with norm (traverse-stx/recur #'stx reinterpret)])

  (define-syntax-class u-type-path
    #:attributes [norm]
    #:literal-sets [u-type-literals]
    [pattern (~#%type:app* (#%type:con dot:#%dot_τ ~!) mp:u-module-path (#%type:con x:id))
             #:with norm (syntax/loc this-syntax (dot mp.norm x))]
    [pattern x:id
             #:with norm #'x])

  (define-syntax-class u-module-path
    #:attributes [norm]
    #:literal-sets [u-type-literals]
    [pattern (~#%type:app* (#%type:con dot:#%dot_m ~!) mp:u-module-path (#%type:con x:id))
             #:with norm (syntax/loc this-syntax (dot mp.norm x))]
    [pattern x:id
             #:with norm #'x])

  (define-syntax-class u-type-app
    #:attributes [norm]
    #:literal-sets [u-type-literals]
    [pattern (~#%type:app* (#%type:con app:#%apply-type ~!) tp:u-type-path arg:u-type ...)
             #:with norm #'(app tp.norm arg.norm ...)])

  ;; Reinterpret the given uninterpreted type, so that the resulting type
  ;; can be expanded into a valid Hackett type.
  ;; UType -> TypeStx
  (define (reinterpret-u-type ut)
    (syntax-parse ut
      [u:u-type #'u.norm]))

  ;; Stx -> Stx
  ;; where UTypes inside are reinterpreted
  (define (reinterpret stx)
    (syntax-parse stx
      [u:u-type #'u.norm]
      [_ (traverse-stx/recur stx reinterpret)]))


  (define (path->u-type-path p)
    (syntax-parse p
      #:literals [#%dot_τ]
      [x:id #'x]
      [(dot:#%dot_τ mp x)
       #:with mp* (path->u-mod-path #'mp)
       (template
        (?#%type:app* (#%type:con dot)
                      mp*
                      (#%type:con x)))]))

  (define (path->u-mod-path p)
    (syntax-parse p
      #:literals [#%dot_m]
      [x:id #'x]
      [(dot:#%dot_m mp x)
       #:with mp* (path->u-mod-path #'mp)
       (template
        (?#%type:app* (#%type:con dot)
                      mp*
                      (#%type:con x)))]))

  )

(module+ test
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
