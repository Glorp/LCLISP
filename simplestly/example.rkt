#lang racket

(require "sugar.rkt"
         "core.rkt")

(define (run-f exp env)
  (let ((t (typeof exp env))
        (e-exp (erase exp))
        (e-env (erase-env env)))
    (annotated (eval e-exp e-env)
               t)))

(define-syntax run
  (syntax-rules ()
    ((_ x) (run-f (parse-typed x) env))))

(run (c->i ((λ (x : Nat) (ifn false x (s x)))
            (s 0))))

(run (c->i (p (p (s (s (s (s 0))))))))

(match (run (λ ((x : Nat) (y : Nat) (z : Bool)) z))
  ((annotated _ t) t))

(run ((λ ((x : Nat) (y : Nat) (z : Bool)) z) 0 0 false))

