#lang racket/base

(provide (signature-out sig
                        Π)
         val
         type
         data
         module
         def-signature
         ;; ---
         #%internal-decl
         (for-syntax internal-decl-struct))

(require syntax/parse/define
         "rep/sig-literals.rkt"
         "rep/apply-type.rkt"
         "namespace/reqprov.rkt"
         (except-in hackett/private/type-language
                    ~type
                    type-namespace-introduce
                    value-namespace-introduce)
         hackett/private/type-reqprov
         (only-in hackett/private/adt data-constructor-spec)
         (only-in (unmangle-in hackett/base) type data)
         (for-syntax racket/base
                     racket/match
                     syntax/parse
                     (only-in syntax/parse [attribute @])
                     syntax/parse/experimental/template
                     hackett/private/util/stx
                     "rep/sig.rkt"
                     "namespace/namespace.rkt"
                     "util/hash.rkt"))


(define-syntax val #f)
(define-syntax module #f)

;; (#%internal-decl #<internal-decl-struct>)
(define-syntax #%internal-decl #f)
(begin-for-syntax
  (struct internal-decl-struct [key internal-id decl]))

(begin-for-syntax
  (define-literal-set sig-surface-literals
    #:datum-literals [: =]
    [val type data module #%internal-decl])

  (define-syntax-class sig-entry
    #:attributes [[key 1] [id 1] [decl 1]]
    #:literal-sets [sig-surface-literals]

    [pattern (#%internal-decl intdeclstruct)
      #:do [(match-define (internal-decl-struct key0 internal-id0 decl0)
              (syntax-e #'intdeclstruct))]
      #:with [key ...] #`[#,key0]
      #:with [id ...] #`[#,internal-id0]
      #:with [decl ...] #`[#,decl0]]

    [pattern (val {~value x:id} : {~type val-type:expr})
      #:with [key ...]  #`[#,(namespaced:value #'x)]
      #:with [id ...]   #'[x]
      #:with [decl ...] #'[(#%val-decl val-type)]]

    [pattern (data {~type X:id} c:data-constructor-spec ...)
      #:with X-key (namespaced:type (attribute X))
      #:with [c-key ...] (map namespaced:value (attribute c.tag))
      #:with [c-tag ...] (map value-namespace-introduce (attribute c.tag))
      #:with [c-type ...] (type-namespace-introduce
                           (template [(?->* c.arg ... X) ...]))
      #:with [[key id decl] ...]
      #`[[X-key X (#%type-decl (#%data [] c-tag ...))]
         [c-key c-tag (#%constructor-decl c-type)]
         ...]]
    [pattern (data {~type (X:id arg:id ...+)} c:data-constructor-spec ...)
      #:with X-key (namespaced:type (attribute X))
      #:with [c-key ...] (map namespaced:value (attribute c.tag))
      #:with [c-tag ...] (map value-namespace-introduce (attribute c.tag))
      #:with [c-type ...] (type-namespace-introduce
                           (template
                            [(?#%type:forall* [arg ...]
                               (?->* c.arg ... (#%apply-type X arg ...)))
                             ...]))
      #:with [[key id decl] ...]
      #`[[X-key X (#%type-decl (#%data [arg ...] c-tag ...))]
         [c-key c-tag (#%constructor-decl c-type)]
         ...]]

    [pattern (type {~type X:id})
      #:with [key ...]  #`[#,(namespaced:type #'X)]
      #:with [id ...]   #'[X]
      #:with [decl ...] #'[(#%type-decl (#%opaque []))]]
    [pattern (type {~type (X:id arg:id ...)})
      #:with [key ...]  #`[#,(namespaced:type #'X)]
      #:with [id ...]   #'[X]
      #:with [decl ...] #'[(#%type-decl (#%opaque [arg ...]))]]

    [pattern (type {~type X:id} = {~type alias-type:expr})
      #:with [key ...]  #`[#,(namespaced:type #'X)]
      #:with [id ...]   #'[X]
      #:with [decl ...] #'[(#%type-decl (#%alias [] alias-type))]]
    [pattern (type {~type (X:id arg:id ...)} = {~type alias-type:expr})
      #:with [key ...]  #`[#,(namespaced:type #'X)]
      #:with [id ...]   #'[X]
      #:with [decl ...] #'[(#%type-decl (#%alias [arg ...] alias-type))]]

    [pattern (module {~module X:id} : {~signature signature:expr})
      #:with [key ...]  #`[#,(namespaced:module #'X)]
      #:with [id ...]   #'[X]
      #:with [decl ...] #'[(#%module-decl signature)]])

  (define-syntax-class sig-entries
    #:attributes [[key 1] [id 1] [decl 1]]
    [pattern [entry:sig-entry ...]
      #:with [key ...] #'[entry.key ... ...]
      #:with [id ...] #'[entry.id ... ...]
      #:with [decl ...] #'[entry.decl ... ...]]))



(define-syntax-parser sig
  [(_ {~value ent} ...)
   #:with entries:sig-entries ((make-syntax-introducer #t) #'[ent ...])
   (define keys (map syntax->datum (attribute entries.key)))
   #`(#%sig
      #,(hash-zip keys (attribute entries.id))
      #,(hash-zip keys (attribute entries.decl)))])

(define-syntax-parser Π
  #:datum-literals [:]
  [(_ ([{~module x} : A:expr]) B:expr)
   #'(#%pi-sig ([x A]) B)])

(define-simple-macro
  (def-signature {~signature name} {~signature sig-expr:sig})
  (begin
    (define-values [] sig-expr.residual)
    (define-syntax name
      (make-variable-like-transformer
       #'sig-expr.expansion))))
