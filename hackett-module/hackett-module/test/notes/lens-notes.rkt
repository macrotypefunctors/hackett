#lang hackett-module

(def-signature TYPE (sig (type T)))

(type Num Double)

(data (Const a b) (Const a))
(instance (∀ [A] (Functor (Const A)))
  [map (λ [f (Const a)]
         (Const a))])

(defn get-const : (∀ [a b] {(Const a b) -> a})
  [[(Const a)] a])

;; ---------------------------------------------------------

(def-signature LENS
  (sig
   (type Lens)
   (type CA) ; s
   (type CB) ; t
   (type A)  ; a
   (type B)  ; b
   (val make-lens : {{CA -> A} -> {CA -> B -> CB} -> Lens})
   (val get : {Lens -> CA -> A})
   (val set : {Lens -> CA -> B -> CB})
   (val modify : {Lens -> CA -> {A -> B} -> CB})))

(def-signature MAKE-LENS
  (Π ([CA : TYPE])    ; s
    (Π ([CB : TYPE])   ; t
      (Π ([A : TYPE])   ; a
        (Π ([B : TYPE])  ; b
          (where
           (where
            (where
             (where
              LENS
              CA = CA.T)
             CB = CB.T)
            A = A.T)
           B = B.T))))))


(def-signature LENS-PAIR
  (sig
   (type InA)
   (type InB)
   (type MidA)
   (type MidB)
   (type OutA)
   (type OutB)
   (module In->Mid :
     (where (where (where (where LENS CA = InA) CB = InB) A = MidA) B = MidB))
   (module Mid->Out :
     (where (where (where (where LENS CA = MidA) CB = MidB) A = OutA) B = OutB))
   (module In->Out :
     (where (where (where (where LENS CA = InA) CB = InB) A = OutA) B = OutB))))

#;
(def-module Lens-Pair
  (λ ([I->M : LENS])
    (λ ([M->O : (where (where LENS CA = I->M.A) CB = I->M.B)])
      (mod
       (type InA I->M.CA) ; CCA
       (type InB I->M.CB) ; CCB
       (type MidA I->M.A) ; CA
       (type MidB I->M.B) ; CB
       (type OutA M->O.A) ; A
       (type OutB M->O.B) ; B
       (def-module In->Mid I->M)
       (def-module Mid->Out M->O)
       (def-module In->Out
         (mod))

       ;; ---
       (: thrush {In->Mid.Lens -> Mid->Out.Lens -> In->Out.Lens})
       (defn thrush
         [[l1 l2]
          (let ([g1 (In->Mid.get l1)]
                [g2 (Mid->Out.get l2)]
                [s1 (In->Mid.set l1)]
                [s2 (Mid->Out.set l2)])
            ;; g1 : CCA -> CA
            ;; g2 : CA -> A
            ;; s1 : CCA -> CB -> CCB
            ;; s2 : CA -> B -> CB
            (In->Out.make-lens
             {g2 |.| g1}
             (λ [cca b]
               (s1 cca (s2 (g1 cca) b)))))])))))
           

;; ---------------------------------------------------------

(def-module Lens1
  (λ ([CA : TYPE])
    (λ ([CB : TYPE])
      (λ ([A : TYPE])
        (λ ([B : TYPE])
          (mod
           (type CA CA.T)
           (type CB CB.T)
           (type A A.T)
           (type B B.T)
           (data Lens
             (Lens {CA -> A}
                   {CA -> B -> CB}))
           (: make-lens {{CA -> A} -> {CA -> B -> CB} -> Lens})
           (: get {Lens -> CA -> A})
           (: set {Lens -> CA -> B -> CB})
           (: modify {Lens -> CA -> {A -> B} -> CB})
           (def make-lens Lens)
           (defn get [[(Lens get set)] get])
           (defn set [[(Lens get set)] set])
           (defn modify
             [[(Lens get set) ca f]
              (set ca (f (get ca)))])))))))

(def-module Lens1*
  (seal Lens1 :> MAKE-LENS))

;; ---------------------------------------------------------

#;
(def-module Pong
  (λ ([Make-Lens : MAKE-LENS])
    (mod
     (data Dir (Vel Num Num))
     (data Pos (Pos Num Num))
     (data Ball (Ball Pos Dir))
     (data Player (Player Pos Dir))
     (data Score (Score Integer Integer))
     (data Game (Game Score Player Player Ball))
     (def reflect-left-wall
       (Lens.modify (Lens.thrush game-ball ball-dir dir-dx) abs))
     (def reflect-right-wall
       (Lens.modify (Lens.thrush game-ball ball-dir dir-dx) (thrush abs neg)))
     )))

;; ---------------------------------------------------------

#;
(def-module Lens2
  (λ ([CA : TYPE])
    (λ ([CB : TYPE])
      (λ ([A : TYPE])
        (λ ([B : TYPE])
          (mod
           (type CA CA.T)
           (type CB CB.T)
           (type A A.T)
           (type B B.T)
           (data Lens
             (L (∀ [f] (Functor f) => {{A -> (f B)} -> {CA -> (f CB)}})))
           (: make-lens
              {{CA -> A}
               ->
               {CA -> B -> CB}
               ->
               Lens})
           (defn make-lens
             [[get set]
              (L (λ [afb ca]
                   {(set ca) <$> (afb (get ca))}))])

           (: get {Lens -> CA -> A})
           ;(: set {Lens -> CA -> B -> CB})
           ;(: modify {Lens -> CA -> {A -> B} -> CB})
           
           (defn get [[(L l) ca] (get-const (l Const ca))])
           #;#;(defn set [[(Lens get set)] set])
           (defn modify
             [[(Lens get set) ca f]
              (set ca (f (get ca)))])))))))
