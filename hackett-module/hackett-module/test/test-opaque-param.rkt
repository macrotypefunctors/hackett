#lang hackett-module

(def-signature C
  (sig
   (type (C A))
   (val make : (∀ [A] {A -> (C A)}))))

(def-module Take-C
  (λ ([M : C])
    (mod
      (type CI (M.C Integer)))))

(def-module C1
  (seal (mod
         (type (C A) A)
         (def make : (∀ [A] {A -> (C A)}) id))
        :>
        C))

(def-signature C*
  (sig
   (data (C A)
     ;; NOTE: remove constructor defn. and the error goes away
     (make A))))