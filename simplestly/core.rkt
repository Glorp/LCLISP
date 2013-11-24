#lang racket
(require "../untyped/core.rkt"
         (only-in "../untyped/sugar.rkt"
                  parse))

(provide (all-from-out "../untyped/core.rkt")
         cnat
         unit
         bool
         funt
         annotated
         param-type
         typeof
         erase
         erase-env
         base-env)

(struct annotated (exp type) #:transparent)
(struct param-type (name type) #:transparent)

(struct unit () #:transparent)
(struct funt (from to) #:transparent)
(struct cnat () #:transparent)
(struct bool () #:transparent)

(define (erase exp)
  (match exp
    ((annotated x _) (erase x))
    ((lambda (param-type p t) exp) (lambda p (erase exp)))
    ((lambda p x) (lambda p (erase x)))
    ((app f a) (app (erase f) (erase a)))
    ((primitive x) exp)
    ((var v) exp)))

(define (erase-env env)
  (map (λ (x) `(,(car x) ,(erase (cadr x)))) env))

(define id (eval (parse (λ (x) x)) '()))
(define true (eval (parse (λ (a c) a)) '()))
(define false (eval (parse (λ (a c) c)) '()))
(define if-f (eval (parse (λ (b a c) ((b a c) (λ (x) x)))) '()))
(define |0| (eval (parse (λ (f x) x)) '()))
(define s (eval (parse (λ (n f x) (f (n f x)))) '()))
(define |0?| (eval (parse (λ (n) (n (λ (_) (λ (a c) c)) (λ (a c) a)))) '()))

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



(define base-env
  `((u ,(annotated id (unit)))
    (true ,(annotated true (bool)))
    (false ,(annotated false (bool)))
    (0 ,(annotated |0| (cnat)))
    (0? ,(annotated |0?| (funt (cnat) (bool))))
    (s ,(annotated s (funt (cnat) (cnat))))
    (p ,(annotated p (funt (cnat) (cnat))))
    (ifn ,(annotated if-f (funt (bool)
                                (funt (funt (unit) (cnat))
                                      (funt (funt (unit) (cnat))
                                            (cnat))))))
    (fixn ,(annotated y (funt (funt (funt (cnat) (cnat)) (funt (cnat) (cnat)))
                              (funt (cnat) (cnat)))))))


(define (typeof exp env)
  (match exp
    ((vari v) (annotated-type (lookup v env)))
    ((annotated _ t) t)
    ((app f a) (typecheck-app (typeof f env)
                              (typeof a env)))
    ((lambda (param-type p t) x)
     (funt t (typeof x (ext-env env p (annotated #f t)))))))

(define (typecheck-app ft at)
  (match ft
    ((funt pt rt)
     (if (equal? pt at)
         rt
         (error (format "Type mismatch. Parameter: ~a. Argument: ~a."
                        pt
                        at))))))

