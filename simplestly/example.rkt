#lang racket

(require "sugar.rkt"
         "core.rkt")

(define (run-f exp env)
  (let ((t (typeof exp env)))
    (annotated (eval exp env)
               t)))

(define-syntax run
  (syntax-rules ()
    ((_ x) (run-f (parse-typed x) env))))

(run (c->i (((λ (x) (ifn false x (s x))) : (-> Nat Nat))
                         (s 0))))

(run (c->i (p (p (s (s (s (s 0))))))))

