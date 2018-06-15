#lang racket/base

(provide disappeared-use
         add-disappeared-use)

(define disappeared-use 'disappeared-use)
(define (add-disappeared-use stx . ids)
  (define old (or (syntax-property stx disappeared-use) '()))
  (syntax-property stx
    disappeared-use
    (append (map syntax-local-introduce ids) old)))
