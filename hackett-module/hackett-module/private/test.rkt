#lang racket/base

(provide define-binary-check)

(require hackett)
(require hackett
         syntax/parse/define
         (submod hackett/private/test shared)
         (submod hackett/private/test untyped)
         (for-syntax racket/base
                     racket/syntax
                     syntax/srcloc
                     hackett/private/util/stx))

(defn check : (∀ [X] {{X -> X -> Bool}
                      -> {X -> String}
                      -> String
                      -> X
                      -> X
                      -> (IO Unit)})
  [[eq show loc-prefix x y]
   (if {x eq y}
       (test-log! test-success)
       (do (println/error
            {loc-prefix ++ "expectation failed:\n"
             ++ "  expected: " ++ (show y) ++ "\n"
             ++ "     given: " ++ (show x)})
           (test-log! test-failure)))])

(define-syntax-parser define-binary-check
  [(_ name {~optional ([X ...] [C ...] T)} eq show)
   #:with name/loc (format-id #'name "~a/loc" #'name)
   #:with [T-ann ...]
   (if (attribute T)
       #'[: (∀ [X ...] C ... => {String -> T -> T -> (IO Unit)})]
       #'[])
   #'(begin
       (def name/loc T-ann ...
         (check eq show))
       (define-syntax name
         (make-check/loc-transformer (quote-syntax name/loc))))])

(begin-for-syntax
  (define (make-check/loc-transformer name/loc)
    (make-variable-like-transformer
     (λ (id)
       (define srcloc-str
         (datum->syntax #'here (source-location->prefix id)))
       #`(#,name/loc #,srcloc-str)))))

