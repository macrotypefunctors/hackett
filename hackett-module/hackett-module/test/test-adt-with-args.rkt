#lang hackett-module/outside
(require hackett/private/test)

(def-module M
  (mod
    (data Posn (Posn Integer Integer))))

(def three-four : M.Posn
  (M.Posn 3 4))

(def three
  (case three-four
    [(M.Posn x y) x]))

(test {three ==! 3})
