#lang racket
(provide (rename-out (module-begin #%module-begin)
                     (top-interaction #%top-interaction)
                     (top #%top))
         
         #%app)
(require "sugar.rkt"
         "core.rkt")

(struct pretty-printer (x)
  #:property prop:custom-write
  (λ (x o w)
    (fprintf o (pretty-print (pretty-printer-x x)))))


(define (prim+1 x)
  (match x ((primitive n) (primitive (+ n 1)))))
(define env `((c->n ,(primitive (λ (c) (eval (app (app c (primitive prim+1))
                                                  (primitive 0))
                                             '()))))))
                                       
(define (set-env! e)
  (set! env e))

(define-for-syntax (intern stx)
  (syntax-case stx ()
    ((_ x)
     (datum->syntax #'c (syntax->datum #'x) #'x))))

(define-syntax (run stx)
  (syntax-case (intern stx) (def)
    ((def v x) #'(set-env! (cons (list 'v (eval (parse x) env))
                                 env)))
    (x #'(pretty-printer (eval (parse x) env)))))

(define-syntax-rule (module-begin form ...)
  (#%module-begin (run form) ...))

(define-syntax-rule (top-interaction . form)
  (run form))

(define-syntax-rule (top . form)
  (run form))
