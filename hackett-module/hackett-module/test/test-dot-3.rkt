#lang hackett-module

(def-signature A (sig (type X) (val x : X)))

(def-module M
  (seal (mod (type X Integer)
             (def x : X 5))
        :>
        A))

(def y : M.X M.x)

