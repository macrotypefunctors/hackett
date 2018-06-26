#lang hackett-module

(require (only-in racket/base begin for-syntax begin-for-syntax))
(require hackett-module/private/test
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax))

;; ---------------------------------------------------------

(def-signature BOOL-REP
  (sig
   (type Bool)
   (val true : Bool)
   (val false : Bool)
   (val if : (∀ [a] {Bool -> a -> a -> a}))))

(def-signature NAT-REP
  (sig
   (type Nat)
   (val z : Nat)
   (val add1 : {Nat -> Nat})
   (val ?sub1 : {Nat -> (Maybe Nat)})))

(define-simple-macro (def-range n start:nat end:nat start-exp inc)
  #:do [(define ids
          (for/list ([i (in-range (syntax-e #'start) (syntax-e #'end))])
            (format-id #'n "~a~a" #'n i #:source #'n #:props #'n)))]
  #:with [n1 ni |...|] ids
  #:with [nim1 |...| _] ids
  (begin
    (def n1 start-exp)
    (def ni (inc nim1))
    |...|))

;; ---------------------------------------------------------

(def-signature BOOL
  (sig
   (include-sig BOOL-REP)
   (val show : {Bool -> String})
   (val eq : {Bool -> Bool -> Bool})
   (val not : {Bool -> Bool})))

(def-signature NAT
  (sig
   (include-sig NAT-REP)
   (type Bool)
   (val show : {Nat -> String})
   (val = : {Nat -> Nat -> Bool})
   (val + : {Nat -> Nat -> Nat})
   (val ?- : {Nat -> Nat -> (Maybe Nat)})))

;; ---------------------------------------------------------

(def-module Extend-Bool
  (λ ([B : BOOL-REP])
    (seal
     (mod
      (type Bool B.Bool)
      (def true B.true)
      (def false B.false)
      (def if : (∀ [a] {Bool -> a -> a -> a})
        B.if)
      ;; ---
      (defn show [[a] (if a "true" "false")])
      (defn not : {Bool -> Bool} [[a] (if a false true)])
      (defn eq [[a b] (if a b (not b))]))
     :>
     (where BOOL Bool = B.Bool))))

(def-module Extend-Nat
  (λ ([N : NAT-REP])
    (λ ([B : BOOL-REP])
      (seal
       (mod
        (type Nat N.Nat)
        (type Bool B.Bool)
        (def z N.z)
        (def add1 N.add1)
        (def ?sub1 N.?sub1)
        ;; ---
        (defn show : {Nat -> String}
          [[a]
           (case (?sub1 a)
             [Nothing "0"]
             [(Just am1) {"(add1 " ++ (show am1) ++ ")"}])])
        (defn = : {Nat -> Nat -> Bool}
          [[a b]
           (case* [(?sub1 a) (?sub1 b)]
             [[Nothing Nothing]       B.true]
             [[(Just am1) (Just bm1)] {am1 = bm1}]
             [[_ _]                   B.false])])
        (defn +
          [[a b]
           (case (?sub1 a)
             [Nothing b]
             [(Just am1) (add1 {am1 + b})])])
        (defn ?-
          [[a b]
           (case* [(?sub1 a) (?sub1 b)]
             [[Nothing    (Just _)]   Nothing]
             [[_          Nothing]    (Just a)]
             [[(Just am1) (Just bm1)] (?- am1 bm1)])]))
       :>
       (where (where NAT Nat = N.Nat) Bool = B.Bool)))))

;; ---------------------------------------------------------

(def-module Test-Bool
  (λ ([B* : BOOL-REP])
    (mod
     (def-module B (Extend-Bool B*))
     (define-binary-check ==!ᵇ (λ (a b) (B.if (B.eq a b) True False)) B.show)

     (def test
       (do
         {B.true ==!ᵇ B.true}
         {B.false ==!ᵇ B.false}
         {(B.not B.true) ==!ᵇ B.false}
         {(B.not B.false) ==!ᵇ B.true}))
     )))

(def-module Test-Nat
  (λ ([N* : NAT-REP])
    (λ ([B* : BOOL-REP])
      (mod
       (def-module B (Extend-Bool B*))
       (def-module N ((Extend-Nat N*) B))
       (define-binary-check ==!ᵇ (λ (a b) (B.if (B.eq a b) True False)) B.show)
       (define-binary-check ==!ⁿ (λ (a b) (B.if (N.= a b) True False)) N.show)

       (defn -!
         [[a b]
          (case (N.?- a b)
            [(Just r) r]
            [Nothing (error! "cannot go below zero")])])

       (def-range n 0 11 N.z N.add1)
       (def test
         (do
           {n0 ==!ⁿ n0}
           {(N.= n5 n4) ==!ᵇ B.false}
           {(N.add1 n0) ==!ⁿ n1}
           {(N.+ n0 n4) ==!ⁿ n4}
           {(N.+ n6 n2) ==!ⁿ n8}
           {(N.+ n3 n7) ==!ⁿ n10}
           {(N.+ n4 n5) ==!ⁿ n9}
           {(-! n5 n4) ==!ⁿ n1}
           {(-! n9 n2) ==!ⁿ n7}
           {(-! n8 n0) ==!ⁿ n8}
           {(-! n6 n6) ==!ⁿ n0}))
       ))))

;; ---------------------------------------------------------

;; Four different representations of Booleans

(def-module Bool-Rep1
  (mod
   (type Bool Integer)
   (def true 1)
   (def false 0)
   (defn if : (∀ [a] {Bool -> a -> a -> a})
     [[1 a _] a]
     [[0 _ b] b]
     [[_ _ _] (error! "bad")])))

(type hkt:Bool Bool)
(def hkt:if if)

(def-module Bool-Rep2
  (mod
   (type Bool hkt:Bool)
   (def true True)
   (def false False)
   (def if : (∀ [a] {Bool -> a -> a -> a}) hkt:if)))

(def-module Bool-Rep3
  (mod
   (data Bool T F)
   (def true T)
   (def false F)
   (defn if : (∀ [a] {Bool -> a -> a -> a})
     [[T a _] a]
     [[F _ b] b])))

(def-module Bool-Rep4
  (mod
   (type Bool (∀ [a] {a -> a -> a}))
   (def true (λ (t e) t))
   (def false (λ (t e) e))
   (def if : (∀ [a] {Bool -> a -> a -> a}) id)))

(def-module Test-Bool1 (Test-Bool Bool-Rep1))
(def-module Test-Bool2 (Test-Bool Bool-Rep2))
(def-module Test-Bool3 (Test-Bool Bool-Rep3))
(def-module Test-Bool4 (Test-Bool Bool-Rep4))

(test (do Test-Bool1.test Test-Bool2.test Test-Bool3.test Test-Bool4.test))

;; ---------------------------------------------------------

;; Many different representations of Natural Numbers

(def-module Nat-Rep1
  (mod
   (type Nat Integer)
   (def z 0)
   (defn add1 [[n] {n + 1}])
   (defn ?sub1 [[0] Nothing] [[n] (Just {n - 1})])))

(def-module Nat-Rep2
  (mod
   (type Nat (List Unit))
   (def z Nil)
   (defn add1 [[n] {Unit :: n}])
   (defn ?sub1
     [[Nil] Nothing]
     [[{_ :: nm1}] (Just nm1)])))

(def-module Nat-Rep3
  (mod
   (type Nat String)
   (def z "0")
   (defn add1 [[n] {"1 " ++ n}])
   (defn ?sub1
     [["0"] Nothing]
     [[n] (case (tail! (string-split " " n))
            [Nil        (error! "bad")]
            [(List s)   (Just s)]
            [{s0 :: ss} (Just (foldl (λ (a b) {a ++ " " ++ b}) s0 ss))])])))

(def-module Nat-Rep4
  (mod
   (data Nat Z (S Nat))
   (def z Z)
   (def add1 : {Nat -> Nat} S)
   (defn ?sub1 : {Nat -> (Maybe Nat)}
     [[Z] Nothing]
     [[(S nm1)] (Just nm1)])))

(def-module Nat-Rep5
  (mod
   (data Nat (N (∀ [r] {r -> {Nat -> r} -> r})))
   (def z (N (λ (z s) z)))
   (defn add1 [[n] (N (λ (z s) (s n)))])
   (defn ?sub1
     [[(N n)] (n Nothing Just)])))

(def-module Nat-Rep6
  (mod
   (def-module Sub
     (seal (mod (type Nat (∀ [r] {r -> {r -> r} -> r}))
                (def z (λ (z s) z))
                (defn add1 [[n] (λ (z s) (s (n z s)))])
                (def do-n-times id))
           :>
           (sig (type Nat)
                (val z : Nat)
                (val add1 : {Nat -> Nat})
                (val do-n-times : (∀ [r] {Nat -> r -> {r -> r} -> r})))))
   (type Nat Sub.Nat)
   (def z Sub.z)
   (def add1 Sub.add1)
   (def lag-init (Tuple Nothing (Just z)))
   (defn lag-next
     : {(Tuple (Maybe Nat) (Maybe Nat)) -> (Tuple (Maybe Nat) (Maybe Nat))}
     [[(Tuple _ (Just a))] (Tuple (Just a) (Just (add1 a)))]
     [[_] (error! "bad")])
   (defn ?sub1
     [[n] (fst (Sub.do-n-times n lag-init lag-next))])))

(def-module Test-Nat1 ((Test-Nat Nat-Rep1) Bool-Rep4))
(def-module Test-Nat2 ((Test-Nat Nat-Rep2) Bool-Rep2))
(def-module Test-Nat3 ((Test-Nat Nat-Rep3) Bool-Rep1))
(def-module Test-Nat4 ((Test-Nat Nat-Rep4) Bool-Rep2))
(def-module Test-Nat5 ((Test-Nat Nat-Rep5) Bool-Rep3))
(def-module Test-Nat6 ((Test-Nat Nat-Rep6) Bool-Rep1))

(test (do Test-Nat1.test Test-Nat2.test Test-Nat3.test
        Test-Nat4.test Test-Nat5.test Test-Nat6.test))

;; ---------------------------------------------------------

