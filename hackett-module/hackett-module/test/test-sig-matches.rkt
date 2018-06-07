#lang hackett-module

(require (only-in racket/base for-syntax))
(require syntax/parse/define
         (for-syntax racket/base))

(define-simple-macro (check-sig-matches A:expr B:expr)
  #:with M (syntax/loc this-syntax Test)
  (def-module Check (λ ([M : A]) (seal M :> B))))

;; -----------------

(check-sig-matches
 (sig (type X = Integer)
      (val x : X))
 (sig (type X)
      (val x : X)))

(check-sig-matches
 (sig (type Y)
      (type X = Y)
      (val x : X))
 (sig (type X)
      (val x : X)))

(check-sig-matches
 (sig (type X = Integer)
      (val x : Integer))
 (sig (type X)
      (val x : X)))

(check-sig-matches
 (sig (type Y)
      (type X = Y)
      (val x : Y))
 (sig (type X)
      (val x : X)))

(check-sig-matches
 (sig (val v : Integer)
      (type X = Integer)
      (type Y = Integer))
 (sig (val v : X)
      (type X)
      (type Y = X)))

(check-sig-matches
 (sig (type X)
      (type Y = X))
 (sig (type Y)))

;; -----------------

(def-signature I  (sig (type t)))
(def-signature I* (sig (type t = Integer)))
(def-signature J  (sig (type s) (type t)))
(def-signature J* (sig (type s) (type t = s)))

(check-sig-matches I* I)
(check-sig-matches J* J)

(check-sig-matches
 (Π ([x : I]) (sig (val v : x.t)))
 (Π ([x : I*]) (sig (val v : x.t))))

(check-sig-matches
 (Π ([x : I]) (sig (val v : x.t)))
 (Π ([x : I*]) (sig (val v : Integer))))

(check-sig-matches
 (Π ([x : J]) (sig (val v : x.t)))
 (Π ([x : J*]) (sig (val v : x.t))))

(check-sig-matches
 (Π ([x : J]) (sig (val v : x.t)))
 (Π ([x : J*]) (sig (val v : x.s))))

(def-module xj* (seal (mod (type s Bool) (type t s)) :> J*))
(check-sig-matches
 (sig (val v : xj*.t))
 (sig (val v : xj*.s)))

;; -----------------

(check-sig-matches
   (Π ((B110 :
        (sig
         (val false : Boool)
         (val true : Boool)
         (val if : (∀ (X) {Boool -> X -> X -> X}))
         (type Boool))))
      (Π ((N112 :
           (sig
            (val z? : {Nat -> Boool})
            (val add1 : {Nat -> Nat})
            (val sub1 : {Nat -> Nat})
            (type Nat)
            (val z : Nat)
            (type Boool = B110.Boool))))
         (sig
          (val = : {N112.Nat -> N112.Nat -> B110.Boool})
          (val + : {N112.Nat -> N112.Nat -> N112.Nat}))))
   (Π ((B17106 :
        (sig
         (val false : Boool)
         (val true : Boool)
         (val if : (∀ (X) {Boool -> X -> X -> X}))
         (type Boool))))
      (Π ((N19108 :
           (sig
            (val z? : {Nat -> Boool})
            (val add1 : {Nat -> Nat})
            (val sub1 : {Nat -> Nat})
            (type Nat)
            (val z : Nat)
            (type Boool = B17106.Boool))))
         (sig
          (val = : {N19108.Nat -> N19108.Nat -> B17106.Boool})
          (val + : {N19108.Nat -> N19108.Nat -> N19108.Nat})))))

