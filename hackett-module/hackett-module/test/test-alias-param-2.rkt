#lang hackett-module

;; ---------------------------------------------------------

(def-signature S
  (sig
   (type (T a))
   (val make-t : (∀ [a] {a -> (T a)}))
   ))

(def-module F
  (λ ([M : S])
    (mod
     (: t5 (M.T Integer))
     (def t5
       (M.make-t 5)))))

(def-module T
  (mod
   (type (T a) a)

   (: make-t (∀ [a] {a -> (T a)}))
   (defn make-t
     [[a] a])
   ))

(def-module FT (F T))
