#lang racket/base

(provide #%sig
         #%pi-sig
         #%val-decl
         #%constructor-decl
         #%type-decl
         #%alias
         #%opaque
         #%data
         (for-syntax sig-literals
                     sig-literal-ids
                     ; ---
                     namespaced namespaced?
                     namespaced-namespace namespaced-symbol
                     NAMESPACE:value namespaced:value
                     NAMESPACE:type  namespaced:type
                     ; ---
                     sig-internal-ids
                     sig-decls
                     decl-type?
                     decl-type-opaque?
                     decl-type-data?
                     decl-val?
                     decl-constructor?))

(require (for-syntax racket/base
                     syntax/parse
                     "../util/stx.rkt"))

;; A Key is a (namespaced Namespace Symbol)
;; A Namespace is one of:
;;  - 'value
;;  - 'type
(begin-for-syntax
  (struct namespaced [namespace symbol] #:prefab)
  (define NAMESPACE:value 'value)
  (define NAMESPACE:type 'type)
  (define (namespaced:value sym) (namespaced NAMESPACE:value sym))
  (define (namespaced:type sym) (namespaced NAMESPACE:type sym))
  )

;; (#%sig
;;   #:hash([key . internal-id]
;;           ...)
;;   #:hash([key . decl]
;;          ...))
;; where the key sets are the same,
;; and all the internal-ids are bound
;; in all the decls
(define-syntax #%sig #f)

;; (#%pi-sig
;;   ([param-id sig])
;;   sig)
(define-syntax #%pi-sig #f)

;; A Decl is one of:
;;  - (#%val-decl Type)
;;  - (#%constructor-decl Type)
;;  - (#%type-decl (#%alias Type))
;;  - (#%type-decl (#%opaque))
;;  - (#%type-decl (#%data Id ...))  ; constructor ids

(define-syntax #%val-decl #f)
(define-syntax #%constructor-decl #f)
(define-syntax #%type-decl #f)
(define-syntax #%alias #f)
(define-syntax #%opaque #f)
(define-syntax #%data #f)

(begin-for-syntax
  (define sig-literal-ids
    (list #'#%sig
          #'#%pi-sig
          #'#%val-decl
          #'#%constructor-decl
          #'#%type-decl #'#%alias #'#%opaque #'#%data))

  (define-literal-set sig-literals
    [#%sig
     #%pi-sig
     #%val-decl
     #%constructor-decl
     #%type-decl #%alias #%opaque #%data]))

;; -----------------------------------------------------------------

(begin-for-syntax

  ;; Sig -> [Hashof Key Identifier]
  (define (sig-internal-ids s)
    (syntax-parse s
      #:literal-sets [sig-literals]
      [(sig:#%sig internal-ids:hash-literal _)
       (attribute internal-ids.value)]))

  ;; Sig -> [Hashof Key Decl]
  (define (sig-decls s)
    (syntax-parse s
      #:literal-sets [sig-literals]
      [(sig:#%sig _ decls:hash-literal)
       (attribute decls.value)]))

  ;; Decl -> Bool
  (define (decl-type? d)
    (syntax-parse d
      [(#%type-decl _) #t]
      [_ #f]))

  ;; Decl -> Bool
  (define (decl-type-opaque? d)
    (syntax-parse d
      #:literal-sets [sig-literals]
      [(#%type-decl (#%opaque)) #t]
      [_ #f]))

  ;; Decl -> Bool
  (define (decl-type-data? d)
    (syntax-parse d
      #:literal-sets [sig-literals]
      [(#%type-decl (#%data . _)) #t]
      [_ #f]))

  ;; Decl -> Bool
  (define (decl-val? d)
    (syntax-parse d
      #:literal-sets [sig-literals]
      [(#%val-decl _) #t]
      [_ #f]))

  ;; Decl -> Bool
  (define (decl-constructor? d)
    (syntax-parse d
      #:literal-sets [sig-literals]
      [(#%constructor-decl _) #t]
      [_ #f]))

  )
