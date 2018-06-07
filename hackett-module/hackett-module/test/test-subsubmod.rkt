#lang hackett-module
(require (only-in racket/base
                  begin-for-syntax
                  for-syntax))
(require (for-syntax racket/base
                     "../namespace/namespace.rkt"
                     "../rep/sig-pretty.rkt"))

(def-signature S
  (sig
    (module Sub :
      (sig
        (type T)
        (module Another :
          (sig
            (type X)))))

    (type R = Sub.T)
    (type Q = Sub.Another.X)
    ))

(begin-for-syntax
  (define S* (signature-namespace-introduce #'S))

  ;; NOTE: `Sub.Another.X` needs to be reintroduced
  (displayln
   (sig->string
    ((syntax-local-value S*) S*)))

  )
