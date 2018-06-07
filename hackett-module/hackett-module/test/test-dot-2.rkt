#lang hackett-module

(def-signature J* (sig (type t = Integer) (type s = t)))

(def-module xj* (seal (mod (type t Integer) (type s Integer)) :> J*))

(def a : xj*.s 5)
