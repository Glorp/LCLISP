#lang racket

(require (only-in "../untyped/sugar.rkt"
                  parse-aux)
         "core.rkt")

(provide parse-typed)

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
    ((_ (x : t)) (annotated (parse-typed x) (parse-type t)))
    ((_ x) (parse-aux parse-typed x))))

