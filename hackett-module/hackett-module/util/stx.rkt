#lang racket/base

(provide for/free-id-table
         for*/free-id-table
         hash-literal
         (rename-out [hash-literal hash-lit])
         generate-temporaries/1num
         generate-temporary/1num)

(require racket/function
         racket/list
         racket/match
         racket/syntax
         syntax/parse/define
         syntax/parse
         syntax/id-table
         syntax/stx)

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

;; Any -> Symbol
(define (anything->temporary-symbol x)
  (cond
    [(syntax? x)
     (anything->temporary-symbol (syntax-e x))]
    [(or (symbol? x)
         (keyword? x)
         (string? x))
     (format-symbol "~a" x)]
    [else 'temp]))

;; [Listof Any] -> [Listof Id]
;; Like generate-temporaries, but trims excess digits
;; at the end of symbols.
(define (generate-temporaries/1num xs)
  (define (symbol-remove-numbers s)
    (match (regexp-match #px"^(.*?)\\d+$" (symbol->string s))
      [(list _ prefix) (string->symbol prefix)]
      [_ s]))
  (generate-temporaries
   (stx-map (compose symbol-remove-numbers
                     anything->temporary-symbol)
            xs)))

;; Any -> Id
(define (generate-temporary/1num x)
  (first (generate-temporaries/1num (list x))))
