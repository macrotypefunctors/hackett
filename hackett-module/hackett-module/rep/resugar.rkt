#lang racket/base

(require racket/contract/base)
(provide
 (contract-out
  [prop:resugar-origin
   (struct-type-property/c
    (-> any/c resugar-origin?))]
  [resugar-origin?
   (-> any/c boolean?)]
  [resugar-origin-sym
   (-> resugar-origin? symbol?)]

  [resugar-origin
   (-> symbol?
       (-> path-syntax? syntax? intdef-ctx-ish? syntax?)
       resugar-origin?)])

 resugar)

(require racket/match
         struct-like-struct-type-property
         "../util/stx-traverse.rkt")

(define (path-syntax? v)
  ;; TODO: actually check its a path
  (syntax? v))

(define (intdef-ctx-ish? v)
  (or (not v)
      (internal-definition-context? v)
      (and (list? v)
           (andmap internal-definition-context? v))))

(define-struct-like-struct-type-property resugar-origin
  [sym
   how-to-resugar])

;; -------------------
;; Id PathStx Any IntDefCtx -> Stx
;; "Resugars" references to `id` in `stx`. Resugaring
;; behavior is based on the id's binding type.
;; The `path` is a path to the expanded version of `id`.
(define (resugar id path stx intdef-ctx)
  (match-define
    (resugar-origin origin-sym to-resugar)
    (syntax-local-value id #f intdef-ctx))

  (let traverse ([stx stx])
    (cond
      [(and (syntax? stx)
            (syntax-property stx origin-sym))
       (to-resugar path stx intdef-ctx)]

      [else
       (traverse-stx/recur stx traverse)])))
