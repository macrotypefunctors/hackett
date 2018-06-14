#lang hackett-module

(def-module M
  (mod
   (type (Lens CA CB A B) {CA -> (Tuple A {B -> CB})})

   (: make-lens (∀ [CA CB A B]
                   {{CA -> A} -> {CA -> B -> CB} -> (Lens CA CB A B)}))
   (defn make-lens
     [[get set ca]
      (Tuple (get ca) (set ca))])

   #|
   (: get (∀ [CA CB A B]
             {(Lens CA CB A B) -> CA -> A}))
   (defn get
     [[l ca] (case (l ca)
               [(Tuple a _) a])])

   (: set (∀ [CA CB A B]
             {(Lens CA CB A B) -> CA -> B -> CB}))
   (defn set
     [[l ca] (case (l ca)
               [(Tuple _ f) f])])

   (: modify (∀ [CA CB A B]
                {(Lens CA CB A B) -> CA -> {A -> B} -> CB}))
   (defn modify
     [[l ca a->b] (case (l ca)
                    [(Tuple a f) (f (a->b a))])])
   |#
   ))

