#lang hackett-module

(def-signature S (sig (type X) (val x : X)))

(def-module F
  (seal
   (λ ([dummy : (sig)])       ; removing this dummy layer here
     (λ ([M : (sig (type T))])
       (mod (type U M.T))))
   :>
   (Π ([dummy : (sig)])        ; and this dummy layer here gets rid of the error
     (Π ([M : (sig (type T))])
       (sig (type U = M.T))))))


(def-module G
  (seal
   (λ ([dummy : (sig)])  ; removing this dummy layer here
     (λ ([M : S])
       (mod
        (def v : M.X M.x))))
   :>
   (Π ([dummy : (sig)])   ; and this dummy layer here gets rid of the error
     (Π ([M : S])
       (sig
        (val v : M.X))))))
