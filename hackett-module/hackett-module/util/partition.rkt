#lang racket/base

(provide partition*
         partition*/hash)

(require racket/match
         (only-in racket/list empty? [partition partition/2]))
(module+ test
  (require rackunit))

;; partition* : [List [X -> Bool #:+ Y] ...]
;;              [Listof X]
;;              ->
;;              [List [Listof Y] ...]
(define (partition* predicates xs)
  (match predicates
    ['()
     (unless (empty? xs)
       (error 'partition*
              "some elements did not match any predicate:\n  ~v"
              xs))
     '()]
    [(cons y? rst)
     (define-values [ys other-xs] (partition/2 y? xs))
     (cons ys (partition* rst other-xs))]))

;; partition*/hash : [List [K V -> Bool #:+ {0:K_i && 1:V_i}] ...]
;;                   [Hashof K V]
;;                   ->
;;                   [List [Hashof K_i V_i] ...]
(define (partition*/hash predicates kv)
  (match predicates
    ['()
     (unless (hash-empty? kv)
       (error 'partition*/hash
              "some elements did not match any predicate:\n  ~v"
              kv))
     '()]
    [(cons kivi? rst)
     (define-values [kivi other] (partition/2/hash kivi? kv))
     (cons kivi (partition*/hash rst other))]))

;; partition/2/hash : [K V -> Bool #:+ {0:KA && 1:VA} #:- {0:KB && 1:VB}]
;;                    [Hashof K V]
;;                    ->
;;                    (values [Hashof KA VA] [Hashof KB VB])
(define (partition/2/hash kava? kv)
  (for/fold ([A (hash)] [B (hash)])
            ([(k v) (in-hash kv)])
    (if (kava? k v)
        (values (hash-set A k v) B)
        (values A                (hash-set B k v)))))

;; ---------------------------------------------------------

(module+ test
  (check-equal? (partition* (list even? odd?) (list 0 1 2 3 4 5 6))
                (list (list 0 2 4 6)
                      (list 1 3 5)))
  (check-equal? (partition* (list boolean? symbol? string?)
                            (list #t 'a 'b #f "c" #f #t #t "d" 'e 'f "g" "h"))
                (list (list #t #f #f #t #t)
                      (list 'a 'b 'e 'f)
                      (list "c" "d" "g" "h")))

  (check-exn #rx"partition\\*: some elements did not match any predicate"
             (λ ()
               (partition* (list boolean? symbol? string?)
                           (list #t 'a 'b #f "c" '(unexpected) #f "d" 'e))))
  )

