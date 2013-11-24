#lang racket

(require (only-in "../untyped/sugar.rkt"
                  parse-aux)
         "core.rkt")

(provide parse-typed
         pretty-print)

(define-syntax parse-type
  (syntax-rules (-> Bool Nat Unit)
    ((_ Bool) (bool))
    ((_ Nat) (cnat))
    ((_ Unit) (unit))
    ((_ (from -> to)) (funt (parse-type from) (parse-type to)))
    ((_ (from -> to -> more ...)) (parse-type (from -> (to -> more ...))))))

(define-syntax parse-typed
  (syntax-rules (:)
    ((_ (λ (p : t) x)) (parse-typed (λ ((p : t)) x)))
    ((_ (λ ((p : t)) x)) (lambda (param-type 'p (parse-type t)) (parse-typed x)))
    ((_ x) (parse-aux parse-typed x))))

(define (prim+1 x)
  (match x ((primitive n) (primitive (+ n 1)))))

(define c->i (primitive (λ (c) (eval (app (app c (primitive prim+1))
                                                  (primitive 0))
                                     '()))))

(define (cnat->string x)
  (match (eval (app c->i x) '())
    ((primitive i) (~a i))))

(define b->s
  (primitive (λ (c) (eval (app (app c (primitive "true"))
                               (primitive "false"))
                          '()))))

(define (bool->string x)
  (match (eval (app b->s x) '())
    ((primitive s) s)))

(define (pretty-print-type t)
  (match t
    ((unit) "Unit")
    ((bool) "Bool")
    ((cnat) "Nat")
    ((funt from to) (format "(~a -> ~a)" (pretty-print-type from) (pretty-print-fun-type to)))))

(define (pretty-print-fun-type t)
  (match t
    ((funt from to) (format "~a -> ~a" (pretty-print-type from) (pretty-print-fun-type to)))
    (x (pretty-print-type x))))

(define (pretty-print x t)
  (format "~a : ~a"
          (match t
            ((unit) "()")
            ((bool) (bool->string x))
            ((cnat) (cnat->string x))
            ((funt _ _) "<function>"))
          (pretty-print-type t)))
  