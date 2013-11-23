#lang racket
(provide lambda
         fun
         vari
         app
         eval
         eval-aux
         get-env
         primitive
         lookup
         ext-env)

(define (lookup name env)
  (cadr (or (assoc name env)
            (error "unbound" name))))

(struct lambda (var exp) #:transparent)
(struct fun (var exp env) #:transparent)
(struct vari (name) #:transparent)
(struct app (fun arg) #:transparent)
(struct get-env ())
(struct primitive (x) #:transparent)

(define (ext-env env name value)
  `((,name ,value) . ,env))

(define (eval-aux eval exp env)
  (match exp
    ((vari name) (lookup name env))
    ((lambda p x) (fun p x env))
    ((app f a) (eval-app eval
                         (eval f env)
                         (eval a env)))
    ((primitive x) exp)
    ((fun _ _ _) exp)
    ((get-env) env)))

(define (eval-app eval f a)
  (match f
    ((fun v x env) (eval x (ext-env env v a)))
    ((primitive f) (f a))))

(define (eval exp env)
  (eval-aux eval exp env))