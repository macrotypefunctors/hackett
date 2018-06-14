#lang racket/base

(provide signature-matches?
         sig-matches?)

(require (for-template "../rep/sig-literals.rkt")
         "module-var.rkt"
         "../rep/sig.rkt"
         "../util/stx.rkt"
         racket/syntax
         syntax/parse
         syntax/id-set
         (only-in syntax/parse [attribute @])
         syntax/parse/experimental/template
         threading
         hackett/private/typecheck
         hackett/private/util/stx
         (for-template (only-in hackett/private/type-alias
                                make-alias-transformer))
         )

;; A Signature is one of:
;;  - Sig
;;  - PiSig

;; ---------------------------------------------------------
;; signature-matches?

;; Signature Signature -> Boolean
;; the signatures should be already expanded.
(define (signature-matches? A B)
  ;; TODO: call either sig-matches or pi-sig-matches depending
  (syntax-parse (list A B)
    #:literal-sets [sig-literals]
    [[(#%sig . _) (#%sig . _)]
     (sig-matches? A B)]
    [[(#%pi-sig . _) (#%pi-sig . _)]
     (pi-sig-matches? A B)]
    [[_ _]
     #f]))

;; ---------------------------------------------------------
;; pi-sig-matches?

;; PiSig PiSig -> Boolean
;; the signatures A and B should already be expanded
(define (pi-sig-matches? A B)
  (define/syntax-parse (_ ([A-x A-in]) A-out) A)
  (define/syntax-parse (_ ([B-x B-in]) B-out) B)
  (and
   ;; the "in" is contravariant
   (signature-matches? #'B-in #'A-in)
   (let ()
     ;; Create a context where both `A-x` and `B-x` are bound
     ;; to the same module with the signature `B-in`.
     ;; It's `B-in` because it has the most specific entries
     ;; that both signatures need to be compatible with.
     (define ctx (syntax-local-make-definition-context))
     (syntax-local-bind-decl #'B-x #'(#%module-decl B-in) ctx)
     (syntax-local-bind-syntaxes
      (list #'A-x)
      #'(make-rename-transformer (quote-syntax B-x))
      ctx)

     (define A-out* (expand-sig #'A-out ctx))
     (define B-out* (expand-sig #'B-out ctx))
     (signature-matches? A-out* B-out*))))

;; ---------------------------------------------------------
;; sig-matches?

;; Sig Sig -> Boolean
;; the signatures should be already expanded.
(define (sig-matches? A B)
  (define/syntax-parse (_ A-internal-ids:hash-lit A-decls:hash-lit) A)
  (define/syntax-parse (_ B-internal-ids:hash-lit B-decls:hash-lit) B)

  ;; A Common is an Identifier to be bound in the extended common
  ;; environment that will be used for checking the components against
  ;; each other

  ;; key->common : [Hashof Key Common]
  (define key->common
    (let*
        ([acc (hash)]
         [acc
          (for/fold ([acc acc])
                    ([k (in-hash-keys (@ A-internal-ids.value))]
                     #:when (not (hash-has-key? acc k)))
            (hash-set acc k (generate-temporary k)))]
         [acc
          (for/fold ([acc acc])
                    ([k (in-hash-keys (@ B-internal-ids.value))]
                     #:when (not (hash-has-key? acc k)))
            (hash-set acc k (generate-temporary k)))])
      acc))

  ;; TODO: make an environment that maps the Common ids to
  ;;       transformers that act like their components from A.

  (define common-ctx
    (syntax-local-make-definition-context))
  (define (intro stx)
    (internal-definition-context-introduce common-ctx stx))

  (define A*
    (intro
     (signature-substs
      A
      (for/free-id-table ([(key id) (in-hash (@ A-internal-ids.value))])
        (values id (hash-ref key->common key))))))

  (define B*
    (intro
     (signature-substs
      B
      (for/free-id-table ([(key id) (in-hash (@ B-internal-ids.value))])
        (values id (hash-ref key->common key))))))

  (for ([(key decl) (in-hash (sig-decls A*))])
    (define common-id (hash-ref key->common key))
    (syntax-local-bind-decl common-id decl common-ctx))

  ;; check that all components in B correspond with
  ;; components in A
  (for/and ([(key B-decl) (in-hash (sig-decls B*))])
    (define A-decl (hash-ref (sig-decls A*) key #f))
    (and A-decl
         (let ()
           (define A-decl* (expand-decl A-decl common-ctx))
           (define B-decl* (expand-decl B-decl common-ctx))
           (sig-entry-matches?
            A-decl*
            B-decl*)))))

;; Id Decl IntDefCtx -> Void
(define (syntax-local-bind-decl id decl ctx)
  (syntax-parse decl
    #:literal-sets [sig-literals]
    [{~or (#%val-decl _)
          (#%constructor-decl _)}
     (syntax-local-bind-syntaxes (list id) #f ctx)]

    [{~or (#%type-decl (#%opaque [x ...]))
          (#%type-decl (#%data [x ...] c ...))}
     (syntax-local-bind-syntaxes (list id) #f ctx)]

    ;; TODO: deal with possible type parameters for aliases
    [(#%type-decl (#%alias [x ...] t))
     (define rhs #'(make-alias-transformer (list (quote-syntax x) ...)
                                           (quote-syntax t)
                                           #f))
     (syntax-local-bind-syntaxes (list id) rhs ctx)]

    [(#%module-decl signature)
     (define internal-id (generate-temporary id))
     (syntax-local-bind-syntaxes (list internal-id) #f ctx)
     (syntax-local-bind-module id internal-id #'signature ctx)]))


;; Decl Decl -> Bool
(define (sig-entry-matches? A B)
  (syntax-parse (list A B)
    #:literal-sets [sig-literals]
    [[(#%val-decl A) (#%val-decl B)]
     (type-matches? #'A #'B)]
    [[(#%constructor-decl A) (#%val-decl B)]
     (type-matches? #'A #'B)]
    [[(#%constructor-decl A) (#%constructor-decl B)]
     (type-equal? #'A #'B)]

    ;; anything matches an opaque type as long as it has the
    ;; same number of type arguments
    [[(#%type-decl (~or (#%opaque [x ...])
                        (#%alias [x ...] _)
                        (#%data [x ...] . _)))
      (#%type-decl (#%opaque [y ...]))]
     (= (length (@ x)) (length (@ y)))]
    ;; TODO: deal with possible type parameters for aliases
    [[(#%type-decl (#%alias [x ...] A)) (#%type-decl (#%alias [y ...] B))]
     (and
      (= (length (@ x)) (length (@ y)))
      (type-equal? (template (?#%type:forall* [x ...] A))
                   (template (?#%type:forall* [y ...] B))))]
    [[(#%type-decl (#%data [x ...] c-A ...))
      (#%type-decl (#%data [y ...] c-B ...))]
     (and
      (= (length (@ x)) (length (@ y)))
      ; use set comparison because order doesn't matter
      (free-id-set=?
       (immutable-free-id-set (@ c-A))
       (immutable-free-id-set (@ c-B))))]

    [[(#%module-decl A) (#%module-decl B)]
     (sig-matches? #'A #'B)]

    [[_ _]
     #false]))

;; Type Type -> Bool
(define (type-equal? A B)
  (and (type-matches? A B)
       (type-matches? B A)))

;; Type Type -> Bool
(define (type-matches? A B)
  (define (exn-type-mismatch? v)
    (and (exn:fail:syntax? v)
         (regexp-match? #rx"typechecker: type mismatch"
                        (exn-message v))))
  (with-handlers ([exn-type-mismatch? (λ (e) #f)])
    ;; TODO: is this the right function to call?
    (type<:! A B #:src (datum->syntax #f #f))))

;; ---------------------------------------------------------

;; make-rename-context : IntDefCtx [Listof [List Id Id]] -> IntDefCtx
(define (make-rename-context parent renamings)
  (define/syntax-parse [[A B] ...] renamings)
  (define ctx (syntax-local-make-definition-context parent))
  (syntax-local-bind-syntaxes
   (@ A)
   ;; create a syntax object that produces multiple values
   #'(values (make-rename-transformer #'B) ...)
   ctx)
  ctx)
