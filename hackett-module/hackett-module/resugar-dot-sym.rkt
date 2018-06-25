#lang racket/base

(provide attach-resugar-dot-sym
         resugar-dot-sym-id  ; syntax-class
         )

(require syntax/parse)

(define stxprop:resugar-dot-sym
  (gensym 'resugar-dot-sym))

(define (attach-resugar-dot-sym stx sym)
  (syntax-property stx stxprop:resugar-dot-sym sym))

(define-syntax-class resugar-dot-sym-id
  #:attributes [sym sym-stx]
  [pattern x:id
           #:attr sym (syntax-property #'x stxprop:resugar-dot-sym)
           #:with sym-stx (attribute sym)
           #:when (symbol? (attribute sym))])

