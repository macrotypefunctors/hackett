#lang hackett-module

(require hackett/data/identity)

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
   (type (Lens CA CB A B))
   (val make-lens : (∀ [CA CB A B]
                       {{CA -> A} -> {CA -> B -> CB} -> (Lens CA CB A B)}))
   (val get : (∀ [CA CB A B] {(Lens CA CB A B) -> CA -> A}))
   (val set : (∀ [CA CB A B] {(Lens CA CB A B) -> CA -> B -> CB}))
   (val modify : (∀ [CA CB A B] {(Lens CA CB A B) -> CA -> {A -> B} -> CB}))
   (val thrush : (∀ [CCA CCB CA CB A B]
                    {(Lens CCA CCB CA CB)
                     ->
                     (Lens CA CB A B)
                     ->
                     (Lens CCA CCB A B)}))))

;; ---------------------------------------------------------

(def-module Lens1
  (mod
   (data (Lens CA CB A B)
     (Lens {CA -> A}
           {CA -> B -> CB}))

   (: make-lens (∀ [CA CB A B]
                   {{CA -> A} -> {CA -> B -> CB} -> (Lens CA CB A B)}))
   (: get (∀ [CA CB A B] {(Lens CA CB A B) -> CA -> A}))
   (: set (∀ [CA CB A B] {(Lens CA CB A B) -> CA -> B -> CB}))
   (: modify (∀ [CA CB A B] {(Lens CA CB A B) -> CA -> {A -> B} -> CB}))
   (def make-lens Lens)
   (defn get [[(Lens get set)] get])
   (defn set [[(Lens get set)] set])
   (defn modify
     [[(Lens get set) ca f]
      (set ca (f (get ca)))])

   (: thrush (∀ [CCA CCB CA CB A B]
                {(Lens CCA CCB CA CB)
                 ->
                 (Lens CA CB A B)
                 ->
                 (Lens CCA CCB A B)}))
   (defn thrush
     [[l1 l2]
      (let ([g1 (get l1)]
            [g2 (get l2)]
            [s1 (set l1)]
            [s2 (set l2)])
        ;; g1 : CCA -> CA
        ;; g2 : CA -> A
        ;; s1 : CCA -> CB -> CCB
        ;; s2 : CA -> B -> CB
        (make-lens
         {g2 |.| g1}
         (λ [cca b]
           (s1 cca (s2 (g1 cca) b)))))])
   ))

#;
(def-module Lens1*
  (seal Lens1 :> LENS))

;; ---------------------------------------------------------

#;
(def-module Pong
  (λ ([Lens : LENS])
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

(def-module Lens2
  (mod
   (data (Lens CA CB A B)
     (L (∀ [f] (Functor f) => {{A -> (f B)} -> {CA -> (f CB)}})))
   (: make-lens (∀ [CA CB A B]
                   {{CA -> A} -> {CA -> B -> CB} -> (Lens CA CB A B)}))
   (defn make-lens
     [[get set]
      (L (λ [afb ca]
           {(set ca) <$> (afb (get ca))}))])

   (: get (∀ [CA CB A B] {(Lens CA CB A B) -> CA -> A}))
   (: set (∀ [CA CB A B] {(Lens CA CB A B) -> CA -> B -> CB}))
   (: modify (∀ [CA CB A B] {(Lens CA CB A B) -> CA -> {A -> B} -> CB}))
           
   (defn get [[(L l) ca] (get-const (l Const ca))])
   (defn set [[ll ca b] (modify ll ca (λ [_] b))])
   (defn modify
     [[(L l) ca f]
      (run-identity (l {Identity |.| f} ca))])

   (: thrush (∀ [CCA CCB CA CB A B]
                {(Lens CCA CCB CA CB)
                 ->
                 (Lens CA CB A B)
                 ->
                 (Lens CCA CCB A B)}))
   (defn thrush
     [[(L l1) (L l2)]
      (L {l1 |.| l2})])))

;; ---------------------------------------------------------

(def-module Lens3
  (mod
   (data (Lens CA CB A B)
     (L {CA -> (Tuple A {B -> CB})}))

   (: make-lens (∀ [CA CB A B]
                   {{CA -> A} -> {CA -> B -> CB} -> (Lens CA CB A B)}))
   (defn make-lens
     [[get set]
      (L (λ [ca] (Tuple (get ca) (set ca))))])

   (: get (∀ [CA CB A B]
             {(Lens CA CB A B) -> CA -> A}))
   (defn get
     [[(L l) ca] (case (l ca)
                   [(Tuple a _) a])])

   (: set (∀ [CA CB A B]
             {(Lens CA CB A B) -> CA -> B -> CB}))
   (defn set
     [[(L l) ca] (case (l ca)
                   [(Tuple _ f) f])])

   (: modify (∀ [CA CB A B]
                {(Lens CA CB A B) -> CA -> {A -> B} -> CB}))
   (defn modify
     [[(L l) ca a->b] (case (l ca)
                        [(Tuple a f) (f (a->b a))])])

   (: lens-thrush (∀ [CCA CCB CA CB A B]
                     {(Lens CCA CCB CA CB)
                      ->
                      (Lens CA CB A B)
                      ->
                      (Lens CCA CCB A B)}))
   (defn lens-thrush
     [[(L l1) (L l2)]
      (L (λ (cca)
           (case (l1 cca)
             [(Tuple ca cb->ccb)
              (case (l2 ca)
                [(Tuple a b->cb)
                 (Tuple a {cb->ccb |.| b->cb})])])))])
   ))

