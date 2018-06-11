#lang hackett-module

(def-signature FLAG
  (sig
   (val yes? : Bool)))

(def-module Put-Submod
  (λ ([F : FLAG])
    (mod
      (def-module Sub F))))

(def-module Check
  (seal Put-Submod
        :>
        (Π ([F : FLAG])
          (sig
            ;; TODO: (where FLAG ....)
            (module Sub : FLAG)
            ))))

;; ---------------------------------------------------------
