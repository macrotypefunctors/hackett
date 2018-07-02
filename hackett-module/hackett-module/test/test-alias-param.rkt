#lang hackett-module

(require hackett-module/private/test)
(define-binary-check ==! ([X] [(Eq X) (Show X)] X) == show)

(def-signature S
  (sig
   (type (T a) = a)

   (val f : (∀ [a] {a -> (T a)}))))

(def-module F
  (λ ([M : S])
    (mod)))

(def-module M
  (mod
   (type (T a) a)

   (defn f : (∀ [a] {a -> (T a)})
     [[a] a])
   ))

