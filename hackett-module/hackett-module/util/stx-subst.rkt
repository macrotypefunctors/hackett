#lang racket/base

(provide stx-substs
         stx-subst)

(require syntax/parse
         syntax/id-table
         "stx-traverse.rkt")

;; Stx [FreeIdTbl Id] -> Stx
(define (stx-substs s mapping)
  (let traverse ([stx s])
    (syntax-parse stx
      [:id
       (free-id-table-ref mapping stx stx)]
      [_
       (traverse-stx/recur stx traverse)])))

;; Stx Id Id -> Stx
(define (stx-subst s x-old x-new)
  (define mapping
    (make-immutable-free-id-table
     (list (cons x-old x-new))))
  (stx-substs s mapping))

