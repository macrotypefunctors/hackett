#lang hackett-module

(def-signature F
  (Π ([M : (sig (type T))])
    (Π ([N : (sig)])
      (sig (type X = M.T)))))


(def-module G
  (λ ([M : (sig (type T))])
    (mod
      (type U M.T))))
