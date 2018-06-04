#lang racket/base

(provide unmangle-in
         type-out
         module-out
         signature-out
         require/unmangle
         #%require/unmangle-only)

(require syntax/parse/define
         (for-syntax racket/base
                     "namespace.rkt"
                     "mangle/mangle-identifier.rkt"
                     "mangle/mangle-reqprov.rkt"))

(begin-for-syntax
  (define-values [mangle-type unmangle-type]
    (make-id-mangler #:prefix "#%hackett-type:"
                     #:introducer type-namespace-introduce))

  (define-values [mangle-module unmangle-module]
    (make-id-mangler #:prefix "#%hackett-module:"
                     #:introducer module-namespace-introduce))

  (define-values [mangle-signature unmangle-signature]
    (make-id-mangler #:prefix "#%hackett-signature:"
                     #:introducer signature-namespace-introduce))

  (define combined-unmangler
    (or/unmangler unmangle-type
                  unmangle-module
                  unmangle-signature))
  )

(define-syntax unmangle-in
  (make-unmangling-require-transformer combined-unmangler))

(define-syntax type-out
  (make-mangling-provide-transformer mangle-type))

(define-syntax module-out
  (make-mangling-provide-transformer mangle-module))

(define-syntax signature-out
  (make-mangling-provide-transformer mangle-signature))

(define-simple-macro (require/unmangle stuff:expr ...)
  (require (unmangle-in stuff ...)))

(define-simple-macro (#%require/unmangle-only stuff:expr ...)
  (require (unmangle-in #:only stuff ...)))

