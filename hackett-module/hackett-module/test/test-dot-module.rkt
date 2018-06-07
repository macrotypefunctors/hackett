#lang hackett-module

(def-module A
  (mod
   (def-module B
     (mod
      (def-module C
        (mod
         (def-module D
           (mod (type T Integer)))))))))

(def-module M A.B.C.D)

(def b1 : A.B.C.D.T 10)
(def b2 : M.T 11)
