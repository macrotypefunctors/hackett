#lang hackett-module

(def-signature C
  (sig
   (type (C A))
   (val make : (∀ [A] {A -> (C A)}))))

(def-module Take-C
  (λ ([M : C])
    (mod
      (type CI (M.C Integer)))))
