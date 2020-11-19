#lang racket/base


(provide (rename-out [parameterize* parameterize]
                     [make-parameter* make-parameter]))

(define (make-parameter* . args)
  (define the-param (apply make-parameter args))
  (case-lambda
    [() (the-param)]
    [(a) (the-param a)]
    [(a b) the-param]))

(define-syntax-rule (parameterize* ([a b] ...) . body)
  (parameterize ([(a #f #f) b] ...) . body))
