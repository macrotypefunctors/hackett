#lang hackett-module

(def-module M
  (mod
    (type X Integer)
    (def simple : X 5)
    ))

(def x M.simple)


(def-module F
  (λ ([A : (sig (type X)
                (val simple : X))])
    A))

(def-module M-sealed (F M))

(def x-sealed M-sealed.simple)
