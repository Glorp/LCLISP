#lang racket
(require "core.rkt")
(provide parse
         make-env
         pretty-print)

(define-syntax parse-aux
  (syntax-rules (λ let)
    ((_ parse (λ (v) x)) (lambda 'v (parse x)))
    ((_ parse (λ (v vs ...) x)) (parse (λ (v) (λ (vs ...) x))))
    ((_ parse (let ((vs as) ...) x)) (parse ((λ (vs ...) x) as ...)))
    ((_ parse (f a)) (app (parse f) (parse a)))
    ((_ parse (f a as ...)) (parse ((f a) as ...)))
    ((_ parse x) (vari 'x))))

(define-syntax parse
  (syntax-rules ()
    ((_ x) (parse-aux parse x))))

(define-syntax parse-w/env
  (syntax-rules (env)
    ((_ env) (get-env))
    ((_ x) (parse-aux parse-w/env x))))

(define empty-env '())

(define-syntax make-env
  (syntax-rules ()
    ((_ (vs as) ...) (make-env empty-env (vs as) ...))
    ((_ an-env (vs as) ...) (eval (parse-w/env (let ((vs as) ...) env)) an-env))))

(define (pretty-print x)
  (match x
    ((lambda p x) (format "(λ (~a~a)" (~a p) (print-lambda-rest x)))
    ((app f a) (format "(~a ~a)" (print-app-f f) (pretty-print a)))
    ((vari x) (~a x))
    ((fun p x _) (pretty-print (lambda p x)))))

(define (print-env e)
  (~a (map (λ (x) (format "(~a ~a)" (car x) (pretty-print (cdr x)))) e)))
                 
(define (print-lambda-rest x)
  (match x
    ((lambda p x) (format " ~a~a" p (print-lambda-rest x)))
    ((fun p x _) (print-lambda-rest (lambda p x)))
    (x (format ") ~a" (pretty-print x)))))

(define (print-app-f f)
  (match f
    ((app f a) (format "~a ~a" (print-app-f f) (pretty-print a)))
    (x (pretty-print x))))


