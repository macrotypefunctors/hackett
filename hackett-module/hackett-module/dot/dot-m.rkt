#lang racket/base

(provide
 (module-out #%dot)
 (for-syntax dot-accessible-path/module
             dot-accessible-path/value
             dot-accessible-path/pattern
             dot-accessible-path/type))

(require syntax/parse/define
         "../namespace/reqprov.rkt"
         "../rep/sig-literals.rkt"
         (for-syntax racket/base
                     (only-in syntax/parse [attribute @])
                     syntax/parse/define
                     "../namespace/namespace.rkt"
                     "../prop-dot-accessible.rkt"
                     "../util/disappeared-use.rkt"))


;; a ModulePath is one of:
;;   - Identifier
;;   - #'(#%dot ModulePath Symbol)
;; represents a path to a module or submodule

;; ---------------------------------------------------------

(begin-for-syntax
  (define-simple-macro
    (define-path-syntax-class name #:attributes [attr:id ...] id-class)
    (define-syntax-class name
      #:attributes [attr ... root]
      #:literals [#%dot]  ; module version only!
      [pattern {~and root:id ~! {~var || id-class}}]
      [pattern (#%dot ~! {~var mp dot-accessible-path/module} x:id)
        #:do [(define key (namespaced:module (syntax-e #'x)))]
        #:with {~var || id-class} ((@ mp.key->id) key)
        #:with root (attribute mp.root)]))

  (define-path-syntax-class dot-accessible-path/module #:attributes [key->id]
    dot-accessible-id/module)

  (define-path-syntax-class dot-accessible-path/value #:attributes [key->id]
    dot-accessible-id/value)

  (define-path-syntax-class dot-accessible-path/pattern #:attributes [key->id]
    dot-accessible-id/pattern)

  (define-path-syntax-class dot-accessible-path/type #:attributes [key->id]
    dot-accessible-id/type))

;; ---------------------------------------------------------

(define-syntax #%dot
  (syntax-parser
    [(_ {~module m:dot-accessible-path/module} ~! x:id)
     #:do [(define key (namespaced:module (syntax-e #'x)))
           (define module-id ((@ m.key->id) key))]
     #:fail-unless module-id
     (format "~a is not bound to a module within ~a"
             (syntax-e #'x)
             (syntax->datum #'m))
     (add-disappeared-use
      module-id
      #'m.root)]))

;; ---------------------------------------------------------
