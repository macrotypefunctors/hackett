#lang hackett-module

(require (only-in racket/base for-syntax))
(require syntax/parse/define
         (for-syntax racket/base))

(define-simple-macro (check-mod-matches M:expr S:expr)
  (def-module Check (seal M :> S)))

;; ---------------------------

(check-mod-matches
 (mod (type X Integer)
      (def x 0))
 (sig (type X = Integer)
      (val x : Integer)))

(check-mod-matches
 (mod (type X Integer)
      (def x 0))
 (sig (type X)
      (val x : X)))

(check-mod-matches
 (mod (type Y String)
      (type X Y)
      (def x "ollo"))
 (sig (type X)
      (val x : X)))

;; ---------------------------

(check-mod-matches
 (mod (data D))
 (sig (data D)))

(check-mod-matches
 (mod (data D C) (def x : D C))
 (sig (data D C) (val x : D)))

(check-mod-matches
 (mod (data D C) (def x C))
 (sig (data D C) (val x : D)))

(check-mod-matches
 (mod (data D C) (defn f : {D -> Integer} [[C] 1]))
 (sig (data D C) (val f : {D -> Integer})))

(check-mod-matches
 (mod (data D C) (defn f [[C] 1]))
 (sig (data D C) (val f : {D -> Integer})))

(check-mod-matches
 (mod (data Nat Z (S Nat))
      (def z : Nat Z)
      (def s : {Nat -> Nat} S))
 (sig (type Nat)
      (val z : Nat)
      (val s : {Nat -> Nat})))

(check-mod-matches
 (mod (data Nat Z (S Nat))
      (def z Z)
      (def s S))
 (sig (type Nat)
      (val z : Nat)
      (val s : {Nat -> Nat})))

;; ---------------------------

