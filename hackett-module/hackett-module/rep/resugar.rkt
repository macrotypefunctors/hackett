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
       (-> syntax? syntax?)
       resugar-origin?)])

 resugar)

(require racket/match
         struct-like-struct-type-property
         "../util/stx-traverse.rkt")

(define-struct-like-struct-type-property resugar-origin
  [sym
   how-to-resugar])

;; -------------------
;; Id Any IntDefCtx -> Stx
;; "Resugars" references to `id` in `stx`. Resugaring
;; behavior is based on the id's binding type.
(define (resugar id stx intdef-ctx)
  (match-define
    (resugar-origin origin-sym to-resugar)
    (syntax-local-value id #f intdef-ctx))

  (let traverse ([stx stx])
    (cond
      [(and (syntax? stx)
            (syntax-property stx origin-sym))
       (to-resugar stx)]

      [else
       (traverse-stx/recur stx traverse)])))
