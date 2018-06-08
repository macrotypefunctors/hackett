#lang racket/base

(provide (signature-out where))

(require syntax/parse/define
         hackett/private/type-language
         "namespace/reqprov.rkt"
         "rep/sig-literals.rkt"
         (for-syntax racket/base
                     racket/match
                     "check/sig-matches.rkt"
                     "rep/sig.rkt"
                     "util/stx.rkt"))

;; A SymbolPath is one of:
;;  - Symbol
;;  - (#%dot SymbolPath Symbol)

(define-syntax-parser where
  #:datum-literals [=]
  [(_ base:sig type-path:expr = {~type τ:type})
   (define sym-path (path->list #'type-path))
   (sig-where/type-path #'base.expansion sym-path #'τ.expansion)])

(begin-for-syntax
  ;; path->list : SymbolPath -> [NEListof Symbol]
  ;; M.x, () -> M, (x) -> (M, x)
  ;; M.N.y, () -> M.N, (y) -> M, (N, y) -> (M, N, y)
  (define (path->list path [acc '()])
    (syntax-parse path
      #:datum-literals [#%dot]
      [x:id (cons (syntax-e #'x) acc)]
      [(#%dot start finish)
       (path->list #'start (cons (syntax-e #'finish) acc))]))

  ;; sig-where/type-path : Sig [NEListof Symbol] Type -> Sig
  (define (sig-where/type-path base path type)
    (match path
      [(list x)
       (sig-where/type base x type)]
      [(cons x rst)
       (sig-update-module base
                          x
                          (λ (old)
                            (sig-where/type-path old rst type)))]))
  
  ;; sig-update-module : Sig Symbol [Signature -> Signature] -> Sig
  (define (sig-update-module base sym transform-sig)
    (syntax-parse base
      #:literal-sets [sig-literals]
      [(sig:#%sig internal-ids:hash-literal decls:hash-literal)
       #`(sig
          internal-ids
          #,(hash-update (attribute decls.value)
                         (namespaced:module sym)
              (λ (prev-decl)
                (syntax-parse prev-decl #:literal-sets [sig-literals]
                  [(mod-decl:#%module-decl old-sig)
                   (define new-sig (transform-sig #'old-sig))
                   (unless (signature-matches? new-sig #'old-sig)
                     (error "a `where` signature must match the original"))
                   #`(mod-decl #,new-sig)]
                  [_
                   (raise-syntax-error #f
                     (format "can't `where` a non-opaque declaration: ~a" sym)
                     base)]))
              (λ ()
                (raise-syntax-error #f
                  (format "can't `where` a non-existent declaration: ~a" sym)
                  base))))])))

