#lang s-exp "lang.rkt"

(def + (λ (a : Nat)
         (fixn (λ (rec+ : (Nat -> Nat))
                 (λ (b : Nat)
                   (ifn (0? b)
                        (λ (_ : Unit) a)
                        (λ (_ : Unit) (s (rec+ (p b))))))))))

+

(+ (s (s 0)) (s (s (s 0))))
