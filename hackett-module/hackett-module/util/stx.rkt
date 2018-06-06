#lang racket/base

(provide for/free-id-table
         for*/free-id-table
         hash-literal
         (rename-out [hash-literal hash-lit]))

(require syntax/parse/define
         syntax/parse
         syntax/id-table)

(define-syntax-class hash-literal
  [pattern stx
    #:attr value (syntax-e #'stx)
    #:when (hash? (attribute value))
    #:attr keys (hash-keys (attribute value))
    #:attr values (hash-values (attribute value))])

(define-simple-macro
  (define-accumulating-for-variant name base-for/fold ([acc init-expr] ...)
    [(body-value ...) next])
  (define-simple-macro (name clauses . body)
    (base-for/fold ([acc init-expr] ...)
                   clauses
      (let-values ([(body-value ...) (let () . body)])
        next))))

(define-simple-macro
  (define-accumulating-for/*-variant name name* ([acc init-expr] ...)
    [(body-value ...) next])
  (begin
    (define-accumulating-for-variant name for/fold ([acc init-expr] ...)
      [(body-value ...) next])
    (define-accumulating-for-variant name* for*/fold ([acc init-expr] ...)
      [(body-value ...) next])))

(define-accumulating-for/*-variant for/free-id-table for*/free-id-table
  ([tbl (make-immutable-free-id-table)])
  [(k v) (free-id-table-set tbl k v)])

