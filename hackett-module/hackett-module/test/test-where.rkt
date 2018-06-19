#lang hackett-module

(def-signature A (sig (type X) (val x : X) (val f : (-> X Integer))))
(def-signature B (sig (type X) (type Y) (val y : Y) (val f : (-> Y X))))
(def-signature C (sig (type X) (val ffy : Integer)))

(def-module A*
  (seal (mod (type X Integer)
             (def x : X 5)
             (def f : (-> X Integer) (λ (x) x)))
        :>
        A))

(def-module B*/A
  (λ ([A : A])
    (seal
     (mod (type X A.X)
          (type Y (-> Integer X))
          (def y : Y (λ (dummy) A.x))
          (def f : (-> Y X) (λ (y) (y 0))))
     :>
     (where B X = A.X))))

(def-module B* (B*/A A*))  ; (where B X = A*.X)


(def-module C*/AB
  (λ ([A : A])
    (λ ([B : (where B X = A.X)])
      (seal
       (mod (type X A.X)
            (def ffy : Integer (A.f (B.f B.y))))
       :>
       (where C X = A.X)))))


(def-module C* (seal ((C*/AB A*) B*) :> C))


(def-signature TYPE (sig (type T)))

(def-signature TYPE-IDENTITY
  (Π ([V : TYPE])
    (where TYPE T = V.T)))

;; ---------------------------------------------------------

(def-signature AB
  (sig
   (type A)
   (type B)))

(def-signature S
  (Π ([T : TYPE])
    (where (where AB A = T.T) B = T.T)))

