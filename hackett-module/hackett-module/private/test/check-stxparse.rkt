#lang racket/base

(provide check-stxparse)

(require rackunit
         syntax/parse
         syntax/parse/define
         syntax/location
         syntax/srcloc
         (for-syntax racket/base))

(define-syntax-parser check-stxparse
  [(_ actual-expr:expr
      {~and {~seq stuff ...}
            {~seq {~alt {~seq #:literals _}
                        {~seq #:literal-sets _}
                        {~seq #:datum-literals _}}
                  ...}}
      expected-pat
      {~seq #:when pass-expr:expr}
      ...)
   #:with stx this-syntax
   (quasisyntax/loc this-syntax
     (test-begin
      (let ([actual-val actual-expr])
       (with-check-info*
        (list (make-check-name 'check-stxparse)
              (make-check-location
               (build-source-location-list (quote-srcloc stx)))
              (make-check-expression 'stx)
              (make-check-actual actual-val)
              (make-check-expected 'expected-pat))
        #,(quasisyntax/loc this-syntax
            (λ ()
              #,(syntax/loc this-syntax
                  (check-true (syntax-parse actual-val
                                stuff ...
                                [expected-pat
                                 (and pass-expr ...)]
                                [_
                                 #false])))))))))])

