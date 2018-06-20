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
     (: t2 (M.T Integer))
     (def t2 M.t1))))

(def-module T
  (mod
   (type (T a) a)

   (: t1 (T Integer))
   (def t1 5)
   ))

(def-module FT (F T))
