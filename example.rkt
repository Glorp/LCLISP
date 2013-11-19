#lang s-exp "core/lang.rkt"

(def id (λ (a) a))
(def pair (λ (a d f) (f a d)))
(def fst (λ (p) (p (λ (a d) a))))
(def snd (λ (p) (p (λ (a d) d))))

(def y (λ (f)
         ((λ (x) (x x))
          (λ (g)
            (f (λ (a) ((g g) a)))))))

(def 0 (λ (f x) x))
(def 1 (λ (f x) (f x)))
(def s (λ (n f x) (f (n f x))))
(def + (λ (a b) (a s b)))
(def 2 (+ 1 1))
(def 4 (+ 2 2))

(def pred (λ (n) (fst (n (λ (p) (pair (snd p)
                                      (s (snd p))))
                         (pair 0 0)))))

(def - (λ (a b) (b pred a)))

(def true (λ (c a) c))
(def false (λ (c a) a))
(def if (λ (p c a) ((p c a) id)))

(def 0? (λ (n) (n (λ (_) false) true)))

(def y (λ (f)
         ((λ (x) (x x))
          (λ (g)
            (f (λ (a) ((g g) a)))))))

(def <= (λ (a b)
          (0? (- a b))))
          
(def fib* (λ (f n)
            (if (<= n (s 0))
                (λ (_) n)
                (λ (_) (+ (f (pred n))
                          (f (pred (pred n))))))))

(def fib (y fib*))

(c->n (fib (+ 4 4)))
