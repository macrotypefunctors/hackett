#lang hackett-module

(require hackett-module/private/test
         (rename-in hackett-module/rep/apply-type
                    [#%apply-type #%hackett-type:#%apply-type]))
(define-binary-check ==! ([X] [(Eq X) (Show X)] X) == show)

(def-module M
  (mod
   (type (Lens CA CB A B) {CA -> (Tuple A {B -> CB})})

   (defn make-lens
     : (∀ [CA CB A B]
          {{CA -> A} -> {CA -> B -> CB} -> (Lens CA CB A B)})
     [[get set]
      (λ [ca] (Tuple (get ca) (set ca)))])

   (defn get
     : (∀ [CA CB A B]
          {(Lens CA CB A B) -> CA -> A})
     [[l ca] (case (l ca)
               [(Tuple a _) a])])

   (defn set
     : (∀ [CA CB A B]
          {(Lens CA CB A B) -> CA -> B -> CB})
     [[l ca] (case (l ca)
               [(Tuple _ f) f])])

   (defn modify
     : (∀ [CA CB A B]
          {(Lens CA CB A B) -> CA -> {A -> B} -> CB})
     [[l ca a->b] (case (l ca)
                    [(Tuple a f) (f (a->b a))])])

   (defn lens-thrush
     : (∀ [CCA CCB CA CB A B]
          {(Lens CCA CCB CA CB)
           ->
           (Lens CA CB A B)
           ->
           (Lens CCA CCB A B)})
     [[l1 l2]
      (λ (cca)
        (case (l1 cca)
          [(Tuple ca cb->ccb)
           (case (l2 ca)
             [(Tuple a b->cb)
              (Tuple a {cb->ccb |.| b->cb})])]))])
   ))

(def-module N
  (mod
   (type (-> a b) (M.Lens a a b b))))

;; (C X) = (Tuple X Y)
(def tuple-fst-lens
  : (∀ [X1 X2 Y] (M.Lens (Tuple X1 Y) (Tuple X2 Y) X1 X2))
  (λ* [[(Tuple fst snd)] (Tuple fst (λ (fst) (Tuple fst snd)))]))

;; (C Y) = (Tuple X Y)
(def tuple-snd-lens
  : (∀ [X Y1 Y2] (M.Lens (Tuple X Y1) (Tuple X Y2) Y1 Y2))
  (λ* [[(Tuple fst snd)] (Tuple snd (λ (snd) (Tuple fst snd)))]))

(def snd-of-fst-lens (M.lens-thrush tuple-fst-lens tuple-snd-lens))

(def l1 : (∀ [X Y] {(Tuple X Y) N.-> X}) tuple-fst-lens)

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

(test {(M.get l1 (Tuple 51 "b")) ==! 51})
(test {(M.set l1 (Tuple 51 "b") 64) ==! (Tuple 64 "b")})

