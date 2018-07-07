#lang hackett-module

(class (C a)
  [c : {a -> Integer}])

(def-signature S
  (sig
   (instance (∀ (a) (C a) => (C (List a))))
   (val x : Integer)
   (val y : Integer)
   (instance (C Integer))))

(def-module M
  (mod
   (instance (C Integer)
     [c (λ (x) {x + 1})])
   (instance (∀ [a] (C a) => (C (List a)))
     [c (λ* [[Nil] 0]
            [[{x :: xs}] {(c x) + (c xs)}])])
   (def x
     (c 5))
   (def y
     (c (List 1 2 3 4)))
   ))

(def-module F
  (λ ([N : S])
    (mod)))

#;(def-module M*
  (seal M :> S))

