#lang hackett-module

(require hackett-module/private/test)
(define-binary-check ==! ([X] [(Eq X) (Show X)] X) == show)

(def-module M
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

;; (C X) = (Tuple X Y)
(: tuple-fst-lens (∀ [X1 X2 Y] (M.Lens (Tuple X1 Y) (Tuple X2 Y) X1 X2)))
(def tuple-fst-lens
  (M.L (λ* [[(Tuple fst snd)] (Tuple fst (λ (fst) (Tuple fst snd)))])))

;; (C Y) = (Tuple X Y)
(: tuple-snd-lens (∀ [X Y1 Y2] (M.Lens (Tuple X Y1) (Tuple X Y2) Y1 Y2)))
(def tuple-snd-lens
  (M.L (λ* [[(Tuple fst snd)] (Tuple snd (λ (snd) (Tuple fst snd)))])))

(def snd-of-fst-lens (M.lens-thrush tuple-fst-lens tuple-snd-lens))

(test {(M.get snd-of-fst-lens (Tuple (Tuple 39 "a") (Tuple 51 "b")))
       ==!
       "a"})

(test {(M.set snd-of-fst-lens (Tuple (Tuple 39 "a") (Tuple 51 "b")) "aaay!")
       ==!
       (Tuple (Tuple 39 "aaay!") (Tuple 51 "b"))})

(test {(M.set snd-of-fst-lens (Tuple (Tuple 39 "a") (Tuple 51 "b"))
              (List 3 1 4 1 6 1 7))
       ==!
       (Tuple (Tuple 39 (List 3 1 4 1 6 1 7)) (Tuple 51 "b"))})

