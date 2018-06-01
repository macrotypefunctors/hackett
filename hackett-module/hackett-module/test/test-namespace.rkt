#lang hackett-module

(def-module M
  (mod
   (type T Integer)
   (def T 0)

   (data One One)))


(def x : M.T M.T)

(def one : M.One M.One)
