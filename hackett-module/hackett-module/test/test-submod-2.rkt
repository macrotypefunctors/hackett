#lang hackett-module

(def-module M
  (mod
   (def-module Sub
     (mod (type T Integer)))

   (def x : Sub.T 4)))

(def x : M.Sub.T M.x)
(def y : Integer M.x)

