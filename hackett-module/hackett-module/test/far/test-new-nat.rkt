#lang hackett-module

(require hackett-module/private/test)
(define-binary-check ==! ([X] [(Eq X) (Show X)] X) == show)

(def-signature S
  (sig
   (type T = Integer)
   (val f : {T -> Integer})))

(def-signature S*
  (sig
   (type T)
   (val f : {T -> Integer})))

(def-module M
  (mod
   (type T Integer)
   (def f : {T -> Integer} (λ (x) 5))))

(def-module M* (seal M :> S*))

(def-signature BOOL-REP
  (sig
   (type Bool)
   (val true : Bool)
   (val false : Bool)
   (val if : (∀ (X) {Bool -> X -> X -> X}))))

(def-signature NAT-REP
  (sig
   (type Bool)
   (type Nat)
   (val z : Nat)
   (val z? : {Nat -> Bool})
   (val add1 : {Nat -> Nat})
   ;; sub1 raises an error if its argument is z
   (val sub1 : {Nat -> Nat})))

(def-signature NAT
  (Π ([B : BOOL-REP])
    (Π ([N : (where NAT-REP Bool = B.Bool)])
      (sig
       ;; equality
       (val = : {N.Nat -> N.Nat -> B.Bool})

       ;; arithmetic
       (val + : {N.Nat -> N.Nat -> N.Nat})))))

(def-module Bool-Rep
  (seal
   (mod
    (type Bool (∀ (X) {X -> X -> X}))
    (def true : Bool (λ (t f) t))
    (def false : Bool (λ (t f) f))
    (def if : (∀ (X) {Bool -> X -> X -> X})
         (λ (b t f) (b t f))))
   :>
   BOOL-REP))

(def-module Nat
  (seal
   (λ ([B : BOOL-REP])
     (λ ([N : (where NAT-REP Bool = B.Bool)])
       (mod
        ;; comparison
        (def = : {N.Nat -> N.Nat -> B.Bool}
             (λ (a b)
               (B.if
                (N.z? a)
                (N.z? b)
                (B.if (N.z? b)
                      B.false
                      (= (N.sub1 a) (N.sub1 b))))))
        ;; arithmetic
        (def + : {N.Nat -> N.Nat -> N.Nat}
             (λ (a b)
               (B.if (N.z? a)
                     b
                     (N.add1 (+ (N.sub1 a) b))))))))
   :>
   NAT))

(def-module Nat-Rep/Int
  (seal
   (mod (type Bool Bool-Rep.Bool)
        (type Nat Integer)
        (def z : Nat 0)
        (def z? : (-> Nat Bool) (λ (x) (if (== x 0) Bool-Rep.true Bool-Rep.false)))
        (def add1 : (-> Nat Nat) (λ (x) (+ x 1)))
        ;; sub1 raises an error if its argument is z
        (def sub1 : (-> Nat Nat) (λ (x) (- x 1))))
   :>
   (where NAT-REP Bool = Bool-Rep.Bool)))

(def-module Nat/Int
  ((Nat Bool-Rep) Nat-Rep/Int))

(def-module TestNat
  (λ ([NR : (where NAT-REP Bool = Bool-Rep.Bool)])
    (mod
     (def-module N ((Nat Bool-Rep) NR))
     (type Bool Bool-Rep.Bool)
     (type Nat NR.Nat)
     (def true : Bool Bool-Rep.true)
     (def false : Bool Bool-Rep.false)
     (def n0 : Nat NR.z)
     (def n1 : Nat (NR.add1 n0))
     (def n2 : Nat (NR.add1 n1))
     (def n3 : Nat (NR.add1 n2))
     (def n4 : Nat (NR.add1 n3))
     (def n5 : Nat (NR.add1 n4))
     (def n6 : Nat (NR.add1 n5))
     (def n7 : Nat (NR.add1 n6))
     (def n8 : Nat (NR.add1 n7))
     (def n9 : Nat (NR.add1 n8))

     (def test
       (do
         {(Bool-Rep.if (N.= (N.+ n3 n5) n8)
                       True
                       False)
          ==!
          True})))))

(def-module TestNat/Int
  (TestNat Nat-Rep/Int))

(test TestNat/Int.test)
