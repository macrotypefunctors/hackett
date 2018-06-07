#lang racket/base

(provide #%dot)

(require syntax/parse/define
         "../namespace/reqprov.rkt"
         "../rep/sig-literals.rkt"
         (for-syntax racket/base
                     (only-in hackett/private/prop-case-pattern-expander
                              prop:case-pattern-expander)
                     "../namespace/namespace.rkt"
                     "../prop-dot-accessible.rkt"
                     (only-in syntax/parse [attribute @])))

(begin-for-syntax
  (define disappeared-use 'disappeared-use)

  (struct proc+case-pat-exp [proc case-pat-trans]
    #:property prop:procedure (struct-field-index proc)
    #:property prop:case-pattern-expander
    (λ (self) (proc+case-pat-exp-case-pat-trans self)))
  )

(define-syntax #%dot
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
