#lang racket/base

(provide #%dot)

(require syntax/parse/define
         "../namespace/reqprov.rkt"
         "../rep/sig-literals.rkt"
         (only-in "dot-m.rkt"
                  dot-accessible-path/value
                  dot-accessible-path/pattern)
         (for-syntax racket/base
                     (only-in hackett/private/prop-case-pattern-expander
                              prop:case-pattern-expander)
                     "../namespace/namespace.rkt"
                     "../prop-dot-accessible.rkt"
                     "../util/disappeared-use.rkt"
                     (only-in syntax/parse [attribute @])))

(begin-for-syntax
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

     [(_ {~module m:dot-accessible-path/value} ~! x:id)
      #:do [(define key (namespaced:value (syntax-e #'x)))
            (define val-id
              ((@ m.key->id) key))]
      #:fail-unless val-id
      (format "~a is not bound to a value within ~a"
              (syntax-e #'x)
              (syntax->datum #'m))
      (add-disappeared-use
       val-id
       #'m.root)])

   ;; as a case-pattern expander
   (syntax-parser
     #:literal-sets [sig-literals]
     [(_ {~module m:dot-accessible-path/pattern} ~! x:id)
      #:do [(define key (namespaced:value (syntax-e #'x)))
            (define pat-id
              ((@ m.key->id) key))]
      #:fail-unless pat-id
      (format "~a is not bound to a pattern within ~a"
              (syntax-e #'x)
              (syntax->datum #'m))
      (add-disappeared-use
       pat-id
       #'m.root)])))
