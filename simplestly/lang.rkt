#lang racket
(provide (rename-out (module-begin #%module-begin)
                     (top-interaction #%top-interaction)
                     (top #%top))
         
         #%app)
(require "sugar.rkt"
         "core.rkt")

(struct pretty-printer (x t)
  #:property prop:custom-write
  (λ (v o w)
    (fprintf o (pretty-print (pretty-printer-x v)
                             (pretty-printer-t v)))))

(define env base-env)

(define (set-env! e)
  (set! env e))

(define-for-syntax (intern stx)
  (syntax-case stx ()
    ((_ x)
     (datum->syntax #'c (syntax->datum #'x) #'x))))

(define (my-eval x)
  (let ((t (typeof x env))
        (res (eval (erase x)
                   (erase-env env))))
    (annotated res t)))
  

(define-syntax (run stx)
  (syntax-case (intern stx) (def)
    ((def v x) #'(match (my-eval (parse-typed x))
                   ((annotated res t)
                    (set-env! (ext-env env
                                       'v 
                                       (annotated res t))))))
    (x #'(match (my-eval (parse-typed x))
           ((annotated res t)
            (pretty-printer res t))))))

(define-syntax-rule (module-begin form ...)
  (#%module-begin (run form) ...))

(define-syntax-rule (top-interaction . form)
  (run form))

(define-syntax-rule (top . form)
  (run form))
