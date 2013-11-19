#lang s-exp "core/lang.rkt"

(def id (λ (a) a))
(def pair (λ (a d f) (f a d)))
(def fst (λ (p) (p (λ (a d) a))))
(def snd (λ (p) (p (λ (a d) d))))


(fst (pair id pair))
(snd (pair id pair))
