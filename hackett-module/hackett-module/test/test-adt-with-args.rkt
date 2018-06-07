#lang hackett-module
(require hackett/private/test)

(def-module M
  (mod
    (type Coord Integer)
    (data Posn (Posn Coord Coord))))

(def three-four : M.Posn
  (M.Posn 3 4))

(def three
  (case three-four
    [(M.Posn x y) x]))

(test {three ==! 3})
