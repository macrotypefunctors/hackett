#lang hackett-module

(require (only-in racket/base for-syntax))
(require syntax/parse/define
         (for-syntax racket/base))

(define-simple-macro (check-sig-matches A:expr B:expr)
  #:with M (syntax/loc this-syntax Test)
  (def-module Check (λ ([M : A]) (seal M :> B))))

(class (C a)
  [c : {a -> Integer}])

;; -----------------

(check-sig-matches
 (sig (type T1 = Integer))
 (sig (type T1)
      (instance (Eq T1))))

(check-sig-matches
 (sig (type T2)
      (instance (Eq T2)))
 (sig (type T2)
      (instance (Eq T2))))

;; -----------------

(check-sig-matches
 (sig (type T3 = Integer)
      (type T4 = (List T3)))
 (sig (type T4)
      (instance (Eq T4))))

(check-sig-matches
 (sig (type T3)
      (instance (Eq T3))
      (type T4 = (List T3)))
 (sig (type T4)
      (instance (Eq T4))))

(check-sig-matches
 (sig
  (type T5)
  (instance (C T5))
  (instance (∀ [a] (C a) => (C (List a)))))
 (sig
  (type T5)
  (instance (C (List T5)))))

(check-sig-matches
 (sig
  (type T6)
  (instance (C Integer))
  (instance (C T6))
  (instance (∀ [a b] (C a) (C b) => (C (Tuple a b))))
  (instance (∀ [a] (C (Tuple a Integer)) => (C (List a)))))
 (sig
  (type T6)
  (instance (C (List T6)))))

;; -----------------

#;
(check-sig-matches
 (Π ([M : (sig (type T) (instance (Eq T)))])
   (sig (type U = M.T)))
 (Π ([M : (sig (type T) (instance (Eq T)))])
   (sig (type U)
        (instance (Eq U)))))

;; -----------------

