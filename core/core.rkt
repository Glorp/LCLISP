#lang racket
(provide lambda
         fun
         vari
         app
         eval
         get-env)

(define (lookup name env)
  (cadr (or (assoc name env)
            (error "unbound" name))))

(struct lambda (var exp) #:transparent)
(struct fun (var exp env) #:transparent)
(struct vari (name) #:transparent)
(struct app (fun arg) #:transparent)
(struct get-env ())

(define (ext-env env name value)
  `((,name ,value) . ,env))

(define (eval exp env)
  (match exp
    ((vari name) (lookup name env))
    ((lambda p x) (fun p x env))
    ((app f a) (eval-app (eval f env)
                         (eval a env)))
    ((get-env) env)))

(define (eval-app f a)
  (match f
    ((fun v x env) (eval x (ext-env env v a)))))
