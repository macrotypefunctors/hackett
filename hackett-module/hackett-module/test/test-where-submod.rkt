#lang hackett-module

(def-signature TYPE
  (sig (type T)))

(def-signature HAS-TYPE
  (sig (module Sub : TYPE)
       (type U = Sub.T)))

(def-module F
  (λ ([H : (where HAS-TYPE Sub.T = Integer)])
    (mod
      (def v : H.U 0)
      ;; H.U should be Integer since H.U = H.Sub.T = Integer
      )))
