#lang racket/base
(provide
 sig⇒
 sig⇐
 (all-from-out "expand-check-prop.rkt"))

(require
 syntax/parse
 hackett/private/util/stx
 debug-scopes
 ; explorer
 "expand-check-prop.rkt"
 "sig-matches.rkt"
 "../rep/sig-pretty.rkt"
 "../util/stx-traverse.rkt"
 "../namespace/namespace.rkt"
 (for-template "../rep/sig-literals.rkt"))

(define (sig⇒ m [ctx #f])
  (define m-
    (local-expand m 'module-begin '() ctx))
  (define sig
    (syntax-local-introduce
     (detach-sig m-)))
  (list m- sig))

;; assume expected is already expanded
(define (sig⇐ stx expected [ctx #f])
  (define/syntax-parse [m- actual] (sig⇒ stx ctx))
  (unless (signature-matches? #'actual expected)
    ;(explore #'actual)
    ;(explore expected)

    (define (look-for-bool stx)
      (define bx (box #f))
      (let trav ([stx stx])
        (if (and (identifier? stx)
                 (eq? (syntax-e stx) 'Bool))
            (begin (set-box! bx stx) stx)
            (traverse-stx/recur stx trav)))
      (or (unbox bx)
          (error "no bool found")))

    (define bool-1 (look-for-bool #'actual))
    (define bool-2 (look-for-bool expected))
    (printf "same? ~a\n" (free-identifier=? bool-1 bool-2))

    (printf "actual in type? ~a\n" (bound-identifier=? bool-1 (type-namespace-introduce bool-1)))
    (printf "expect in type? ~a\n" (bound-identifier=? bool-2 (type-namespace-introduce bool-2)))

    (raise-syntax-error #f
      (format "signature mismatch\n  expected: ~a\n  given:    ~a"
              #;(sig->string expected 12)
              (+scopes expected)
              #;(sig->string #'actual 12)
              (+scopes #'actual))
      stx))
  #'m-)
