#lang hackett-module

(def-signature ORDERED
  (sig
   (type T)
   (val lt : {T -> T -> Bool})
   (val eq : {T -> T -> Bool})))

(def-signature DICT
  (sig
   (module Key : ORDERED)
   (type Key = Key.T)
   (type Val)
   (type Dict)
   (val empty : Dict)
   (val insert : {Dict -> Key -> Val -> Dict})
   (val lookup : {Dict -> Key -> (Maybe Val)})))

(def-signature INT-DICT
  (where DICT Key.T = Integer))

(def-signature STRING-DICT
  (where DICT Key.T = String))

