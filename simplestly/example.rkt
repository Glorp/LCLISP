#lang s-exp "lang.rkt"

(def ->b (λ ((b : Bool) (_ : Unit)) b))

(def ->n (λ ((n : Nat) (_ : Unit)) n))

(def or (λ ((a : Bool) (b : Bool))
          (ifb a (->b true) (->b b))))

(def pair (λ ((a : Nat) (d : Nat) (b : Bool))
            (ifn b
                 (->n a)
                 (->n d))))

(def fst (λ (p : (Bool -> Nat)) (p true)))

(def snd (λ (p : (Bool -> Nat)) (p false)))

(def 0? (λ (n : Nat)
          (recb n
                (λ (_ : Bool) false)
                true)))

(def + (λ ((a : Nat) (b : Nat)) (recn a s b)))

(+ (s (s 0)) (s (s (s 0))))

(def fib (fixn (λ ((recfib : (Nat -> Nat)) (n : Nat))
                 (ifn (or (0? n) (0? (p n)))
                      (->n n)
                      (λ (_ : Unit) (+ (recfib (p n))
                                       (recfib (p (p n)))))))))

(fib (+ (s (s (s (s 0)))) (s (s (s (s 0))))))

