#lang hackett-module

(def-module M
  (seal (mod (type T Integer)
             (def x 0))
        :>
        (sig (type T)
             (val x : T))))

(def-module M* M)

(def y : M.T
  M*.x)

(def-module Check1
  (seal (mod (def-module S1
               (seal (mod (type T Integer)) :> (sig (type T))))
             (def-module S2 S1))
        :>
        (sig (module S1 : (sig (type T)))
             (module S2 : (sig (type T = S1.T))))))

(def-module N
  (mod
   (def-module S
     (seal (mod (type T Integer)) :> (sig (type T))))))

(def-module Check2
  (seal N.S
        :>
        (sig (type T = N.S.T))))
