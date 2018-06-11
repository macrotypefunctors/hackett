#lang hackett-module

(def-signature TYPE (sig (type T)))

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

;; ---------------------------------------------------------

(def-module IntOrdered1
  (seal (mod (type T Integer) (def lt <) (def eq : {T -> T -> Bool} ==))
        :>
        (where ORDERED T = Integer)))

(def-module IntOrdered2
  (seal (mod (type T Integer) (def lt >) (def eq : {T -> T -> Bool} ==))
        :>
        (where ORDERED T = Integer)))

;; ---------------------------------------------------------

(def-module Make-Dict
  (λ ([K : ORDERED])
    (λ ([V : TYPE])
      (mod
       (def-module Key K)
       (type Key Key.T)
       (type Val V.T)
       (type Dict (List (Tuple Key Val)))

       (: empty Dict)
       (def empty Nil)

       (: insert {Dict -> Key -> Val -> Dict})
       (defn insert 
         [[d k v] {(Tuple k v) :: d}])

       (: lookup {Dict -> Key -> (Maybe Val)})
       (defn lookup
         [[Nil lk] Nothing]
         [[{(Tuple dk dv) :: rst} lk]
          (if (Key.eq dk lk)
              (Just dv)
              (lookup rst lk))])))))

(def-module D
  (seal Make-Dict
        :>
        (Π ([K : ORDERED])
          (Π ([V : TYPE])
            (where (where DICT Key.T = K.T) Val = V.T)))))

;; ---------------------------------------------------------

