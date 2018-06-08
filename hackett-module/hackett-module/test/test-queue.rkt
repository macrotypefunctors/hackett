#lang hackett-module

(require hackett/private/test
         hackett-module/private/test)

(define-binary-check ==! ([X] [(Eq X) (Show X)] X) == show)

(data (Thing T)
  (Thing T {T -> T -> Bool} {T -> String}))
(instance (∀ [T] (Eq (Thing T)))
  [== (λ* [[(Thing a == show) (Thing b _ _)] {a == b}])])
(instance (∀ [T] (Show (Thing T)))
  [show (λ* [[(Thing a == show)] (show a)])])

(def-signature TYPE (sig (type T)))
(def-signature TYPE/Testable
  (sig (type T)
       (val ==_T : {T -> T -> Bool})
       (val show_T : {T -> String})
       (val a : T)
       (val b : T)
       (val c : T)
       (val d : T)
       (val e : T)
       (val f : T)
       (val g : T)))

(def-signature QUEUE
  (sig
   (type Elem)
   (type Queue)
   (val empty : Queue)
   (val insert : {Elem -> Queue -> Queue})
   (val remove : {Queue -> (Maybe (Tuple Elem Queue))})))

;; ---------------------------------------------------------

(def-module Make-Queue-Equality
  (λ ([Q : (where QUEUE Elem = Integer)])
    (mod
     (type Elem Q.Elem)
     (type Queue (Thing Q.Queue))
     (defn ==_q [[xs ys]
                 (case* [(Q.remove xs) (Q.remove ys)]
                   [[Nothing Nothing] True]
                   [[(Just (Tuple x xs)) (Just (Tuple y ys))]
                    {{x == y} && {xs ==_q ys}}]
                   [[_ _] False])])
     (defn show_q [[xs]
                   (case (Q.remove xs)
                     [Nothing " "]
                     [(Just (Tuple x xs)) {(show x) ++ " " ++ (show_q xs)}])])

     (def empty (Thing Q.empty ==_q show_q))
     (defn insert
       [[x (Thing q == show)] (Thing (Q.insert x q) == show)])
     (defn remove
       [[(Thing q == show)]
        (case (Q.remove q)
          [Nothing            Nothing]
          [(Just (Tuple x q)) (Just (Tuple x (Thing q == show)))])])
     )))

(def-module Test-IntQueue
  (λ ([Queue : (where QUEUE Elem = Integer)])
    (mod
     (def-module Q (Make-Queue-Equality Queue))
     (defn remove!
       [[q] (case (Q.remove q)
              [(Just (Tuple _ rst)) rst]
              [_ (error! "cannot remove from an empty queue")])])

     (def test
       (do
         {(Q.remove Q.empty) ==! Nothing}
         {(Q.remove (Q.insert 2 Q.empty)) ==! (Just (Tuple 2 Q.empty))}

         {(Q.remove (Q.insert 8 (Q.insert 2 Q.empty)))
          ==!
          (Just (Tuple 2 (Q.insert 8 Q.empty)))}

         {(Q.remove (Q.insert 15 (Q.insert 8 (Q.insert 2 Q.empty))))
          ==!
          (Just (Tuple 2 (Q.insert 15 (Q.insert 8 Q.empty))))}

         {(Q.remove (Q.insert 15 (remove! (Q.insert 8 (Q.insert 2 Q.empty)))))
          ==!
          (Just (Tuple 8 (Q.insert 15 Q.empty)))}
         ))
     )))

;; ---------------------------------------------------------

(def-module Make-Queue1
  (λ ([Elem : TYPE])
    (seal
     (mod
      (type Elem Elem.T)
      (type Queue (Tuple (List Elem) (List Elem)))
      (def empty (Tuple Nil Nil))
      (defn insert [[x (Tuple back front)] (Tuple {x :: back} front)])
      (defn remove
        [[(Tuple Nil Nil)]           Nothing]
        [[(Tuple back Nil)]          (remove (Tuple Nil (reverse back)))]
        [[(Tuple back {f :: nexts})] (Just (Tuple f (Tuple back nexts)))]))
     :>
     (where QUEUE Elem = Elem.T))))

(def-module Make-Queue2
  (λ ([Elem : TYPE])
    (seal
     (mod
      (type Elem Elem.T)
      (type Queue (List Elem))
      (def empty Nil)
      (defn insert [[x q] {q ++ (List x)}])
      (defn remove
        [[Nil]           Nothing]
        [[{f :: nexts}]  (Just (Tuple f nexts))]))
     :>
     (where QUEUE Elem = Elem.T))))

;; ---------------------------------------------------------

(def-module Int/Testable
  (mod
   (type T Integer)
   (def ==_T : {T -> T -> Bool} ==)
   (def show_T : {T -> String} show)
   (def a 5)
   (def b 4)
   (def c 6)
   (def d 3)
   (def e 7)
   (def f 2)
   (def g 8)))

(def-module IntQueue1 (Make-Queue1 Int/Testable))
(def-module IntQueue2 (Make-Queue2 Int/Testable))

(def-module Test-IntQueue1 (Test-IntQueue IntQueue1))
(def-module Test-IntQueue2 (Test-IntQueue IntQueue2))

(test (do
        Test-IntQueue1.test
        Test-IntQueue2.test))

