#lang hackett-module

(def-signature FLAG
  (sig
   (val yes? : Bool)))

(def-module Yes
  (mod
    (def yes? True)))

;; ---------------------------------------------------------

(def-module Flag-Submod
  (mod
    (def-module Sub Yes)))

(def-module Check1
  (seal Flag-Submod
        :>
        (sig
          (module Sub : FLAG))))

;; ---------------------------------------------------------

(def-module Put-Submod
  (λ ([F : FLAG])
    (mod
      (def-module Sub F))))

(def-module Check2
  (seal Put-Submod
        :>
        (Π ([F : FLAG])
          (sig
            (module Sub : FLAG)
            ))))

;; ---------------------------------------------------------
