#lang hackett-module

(def-signature SING (sig (type T) (val x : T)))

(def-module M
  (mod
    (def-module Sub
      (seal (mod (type T Integer)
                 (def x 4))
            :>
            SING))

    ;; expected: (val y : Sub.T) --- *not* opaque:Sub.T1234
    (def y Sub.x)))
