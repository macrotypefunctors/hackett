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
        (val t : T)))

    (type R = Sub.T)
    (val r : R)
    ))

(def-module M
  (seal (mod
          (def-module Sub
            (mod
              (type T Integer)
              (def t 4)))
          (type R Sub.T)
          (def r 5))
        :>
        S))

(def from-submod : M.Sub.T
  M.r)

(def also-from-submod : M.R
  M.Sub.t)

;; --------------------------------------

(def-signature H
  (Π ([M : (sig (type T = Integer))])
    (sig
      (type U = M.T))))

(begin-for-syntax
  (define S* (signature-namespace-introduce #'S))
  (define H* (signature-namespace-introduce #'H))

  ;; NOTE: `Sub.T` needs to be reintroduced
  (displayln
   (sig->string
    ((syntax-local-value S*) S*)))

  ;; NOTE: `M.T` reference should be preserved
  (displayln
   (sig->string
    ((syntax-local-value H*) H*)))

  )
