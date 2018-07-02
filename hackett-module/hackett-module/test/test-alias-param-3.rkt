#lang hackett-module

;; ---------------------------------------------------------

(def-signature S
  (sig
   (type (T a))
   (val t1 : (T Integer))
   ))

(def-module F
  (λ ([M : S])
    (mod
     (def t2 : (M.T Integer) M.t1))))

(def-module T
  (mod
   (type (T a) a)

   (def t1 : (T Integer) 5)
   ))

(def-module FT (F T))
