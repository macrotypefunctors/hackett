#lang racket/base

(provide #%apply-type
         (for-syntax non-currying-type-transformer?
                     gen:non-currying-type-transformer
                     ?#%apply-type
                     ~#%apply-type
                     u-type-literals
                     path->u-type-path u-type-path->path
                     path->u-mod-path u-mod-path->path
                     u-type-app->type))

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
             racket/generic
             (only-in syntax/parse [attribute @])
             syntax/parse/experimental/template
             syntax/parse/class/local-value
             syntax/apply-transformer
             hackett/private/util/stx
             "../namespace/namespace.rkt"
             "../util/stx-traverse.rkt"
             "../util/disappeared-use.rkt"
             (for-syntax racket/base
                         syntax/parse)))


(begin-for-syntax

  (define-generics non-currying-type-transformer
    #:defaults ([alias-transformer?]))

  (define-syntax-class applicable-type
    #:attributes [apply]
    [pattern {~var head (local-value non-currying-type-transformer?)}
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

;; -------------------------------------------------------------------
;; "Uninterpreted" paths / type application

;; Types are expanded into an "uninterpreted" form so that they can
;; be re-expanded without causing problems, and then converted back into
;; unexpanded syntax that will have meaning in a different context.

;; A UTypePath is one of:
;;   - Id
;;   - (#%type:app* (#%type:con #%dot_τ) UModulePath (#%type:con Symbol))

;; A UModulePath is one of:
;;   - Id
;;   - (#%type:app* (#%type:con #%dot_m) UModulePath (#%type:con Symbol))

;; A UTypeApp is a:
;;   (#%type:app* (#%type:con #%apply-type)
;;                UTypePath
;;                Type
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

  (define-syntax ~#%apply-type
    (pattern-expander
     (syntax-parser
       [(_ . lpat)
        (syntax/loc this-syntax
          (~#%type:app* ({~literal #%type:con} {~literal #%apply-type})
                        .
                        lpat))])))

  (define-syntax-class u-type-path
    #:attributes [norm]
    #:literal-sets [u-type-literals]
    [pattern (~#%type:app* (#%type:con dot:#%dot_τ ~!) mp:u-module-path (#%type:con x:id))
             #:with stx this-syntax
             #:with norm (datum->syntax #'stx (list #'dot #'mp.norm #'x) #'stx)]
    [pattern x:id
             #:with norm #'x])

  (define-syntax-class u-module-path
    #:attributes [norm]
    #:literal-sets [u-type-literals]
    [pattern (~#%type:app* (#%type:con dot:#%dot_m ~!) mp:u-module-path (#%type:con x:id))
             #:with stx this-syntax
             #:with norm (datum->syntax #'stx (list #'dot #'mp.norm #'x) #'stx)]
    [pattern x:id
             #:with norm #'x])

  (define-syntax-class u-type-app
    #:attributes [norm]
    #:literal-sets [u-type-literals]
    [pattern (~#%type:app* (#%type:con app:#%apply-type ~!) tp:u-type-path arg ...)
             #:with stx this-syntax
             #:with norm
             (datum->syntax #'stx
                            (list* #'app #'tp.norm #'[arg ...])
                            #'stx)])

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

  (define (u-type-path->path up)
    (syntax-parse up
      [u:u-type-path #'u.norm]))

  (define (u-mod-path->path mp)
    (syntax-parse mp
      [u:u-module-path #'u.norm]))

  (define (u-type-app->type ua)
    (syntax-parse ua
      [u:u-type-app #'u.norm]))

  )
