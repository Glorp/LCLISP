#lang racket
(require "../untyped/core.rkt"
         (only-in "../untyped/sugar.rkt"
                  parse))

(provide cnat
         unit
         bool
         funt
         int
         annotated
         eval
         typeof
         erase
         erase-env
         env)

(struct annotated (exp type) #:transparent)
(struct no-really (exp type) #:transparent)


(struct unit () #:transparent)
(struct funt (from to) #:transparent)
(struct cnat () #:transparent)
(struct bool () #:transparent)
(struct int () #:transparent)

(define (erase exp)
  (match exp
    ((annotated x _) (erase x))
    ((no-really x _) (erase x))
    ((lambda p x) (lambda (erase p) (erase x)))
    ((app f a) (app (erase f) (erase a)))
    ((primitive x) exp)
    ((var v) exp)))

(define (erase-env env)
  (map (λ (x) `(,(car x) ,(erase (cadr x)))) env))

(define true (eval (parse (λ (a c) a)) '()))
(define false (eval (parse (λ (a c) c)) '()))
(define if-f (eval (parse (λ (b a c) (b a c))) '()))
(define |0| (eval (parse (λ (f x) x)) '()))
(define s (eval (parse (λ (n f x) (f (n f x)))) '()))
(define y (eval (parse (λ (f)
                         ((λ (x) (x x))
                          (λ (g)
                            (f (λ (a) ((g g) a)))))))
                '()))

(define p (eval (parse (λ (n) ((λ (p) (p (λ (a d) a)))
                               (n (λ (p) ((λ (a d f) (f a d))
                                          ((λ (p) (p (λ (a d) d))) p)
                                          ((λ (n f x) (f (n f x))) ((λ (p) (p (λ (a d) d))) p))))
                                  ((λ (a d f) (f a d)) (λ (f x) x) (λ (f x) x))))))
                '()))

(define (prim+1 x)
  (match x ((primitive n) (primitive (+ n 1)))))
(define c->i (primitive (λ (c) (eval (app (app c (primitive prim+1))
                                                  (primitive 0))
                                     '()))))

(define env `((true ,(no-really true (bool)))
              (false ,(no-really false (bool)))
              (0 ,(no-really |0| (cnat)))
              (s ,(no-really s (funt (cnat) (cnat))))
              (p ,(no-really p (funt (cnat) (cnat))))
              (ifn ,(no-really if-f (funt (bool)
                                        (funt (cnat)
                                              (funt (cnat)
                                                    (cnat))))))
              (fixn ,(no-really y (funt (funt (funt (cnat) (cnat)) (funt (cnat) (cnat)))
                                        (funt (cnat) (cnat)))))
              (c->i ,(no-really c->i (funt (cnat) (int))))))


(define (typeof exp env)
  (match exp
    ((vari v) (type-from-an (lookup v env)))
    ((no-really _ t) t)
    ((app f a) (typecheck-app (typeof f env)
                              (typeof a env)))
    ((annotated (lambda p x) (funt from to))
     (funt from (typecheck x to (ext-env env p (no-really #f from)))))
    ((annotated x t) (typecheck x t env))))

(define (typecheck exp expected env)
  (let ((type (typeof exp env)))
    (if (equal? type expected)
        type
        (error (format "Type mismatch. Found: ~a. Expected: ~a."
                       type
                       expected)))))


(define (typecheck-app ft at)
  (match ft
    ((funt pt rt)
     (if (equal? pt at)
         rt
         (error (format "Type mismatch. Parameter: ~a. Argument: ~a."
                        pt
                        at))))))

(define (type-from-an exp)
  (match exp
    ((annotated _ t) t)
    ((no-really _ t) t)))

