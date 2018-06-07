#lang racket/base

(provide
 prop:dot-origin             dot-origin             dot-origin?
 dot-origin-module-sym
 prop:dot-accessible/value   dot-accessible/value   dot-accessible/value?
 prop:dot-accessible/pattern dot-accessible/pattern dot-accessible/pattern?
 prop:dot-accessible/type    dot-accessible/type    dot-accessible/type?
 prop:dot-accessible/module  dot-accessible/module  dot-accessible/module?
 prop:dot-accessible         dot-accessible         #| no predicate |#
 ;; ----------------
 dot-accessible-id/value
 dot-accessible-id/pattern
 dot-accessible-id/type
 dot-accessible-id/module
 )

(require struct-like-struct-type-property
         syntax/parse
         syntax/parse/class/local-value)

;; module-sym : Symbol
;; this is the symbol that `reintroduce-#%dot` uses to compare

(define-struct-like-struct-type-property dot-origin
  [module-sym])

;; key->id : Key -> [Maybe Id]
;; such that the id will expand to someting that has/is the
;; correct type already

(define-struct-like-struct-type-property dot-accessible/value
  [key->id])

(define-struct-like-struct-type-property dot-accessible/pattern
  [key->id])

(define-struct-like-struct-type-property dot-accessible/type
  [key->id])

(define-struct-like-struct-type-property dot-accessible/module
  [key->id])

(define-struct-like-struct-type-property dot-accessible
  [module-sym
   value-key->id
   pattern-key->id
   type-key->id
   module-key->id]
  #:property prop:dot-origin
  (λ (self) (dot-origin (dot-accessible-module-sym self)))
  #:property prop:dot-accessible/value
  (λ (self) (dot-accessible/value (dot-accessible-value-key->id self)))
  #:property prop:dot-accessible/pattern
  (λ (self) (dot-accessible/pattern (dot-accessible-pattern-key->id self)))
  #:property prop:dot-accessible/type
  (λ (self) (dot-accessible/type (dot-accessible-type-key->id self)))
  #:property prop:dot-accessible/module
  (λ (self) (dot-accessible/module (dot-accessible-module-key->id self))))

;; ---------------------------------------------------------

(define-syntax-class dot-accessible-id/value
  #:attributes [key->id]
  [pattern {~var id (local-value dot-accessible/value?)}
    #:attr key->id (dot-accessible/value-key->id (attribute id.local-value))])

(define-syntax-class dot-accessible-id/pattern
  #:attributes [key->id]
  [pattern {~var id (local-value dot-accessible/pattern?)}
    #:attr key->id (dot-accessible/pattern-key->id (attribute id.local-value))])

(define-syntax-class dot-accessible-id/type
  #:attributes [key->id]
  [pattern {~var id (local-value dot-accessible/type?)}
    #:attr key->id (dot-accessible/type-key->id (attribute id.local-value))])

(define-syntax-class dot-accessible-id/module
  #:attributes [key->id]
  [pattern {~var || (local-value dot-accessible/module?)}
    #:attr key->id (dot-accessible/module-key->id (attribute local-value))])

