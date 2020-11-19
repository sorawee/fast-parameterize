#lang racket/base

(provide make-parameter
         parameterize)

(require syntax/parse/define
         (for-syntax racket/base))

(define make-parameter
  (case-lambda
    [(init guard)
     (let ([v (guard init)])
       (case-lambda
         [() v]
         [(u) (set! v (guard u))]))]
    [(init)
     (make-parameter init (lambda (x) x))]))

(define-syntax-parser parameterize
  [(_ () e1 e2 ...) #'(let () e1 e2 ...)]
  [(_ ([x v] ...) e1 e2 ...)
   #:with (p ...) (generate-temporaries #'(x ...))
   #:with (y ...) (generate-temporaries #'(x ...))
   #'(let ({~@ [p x] [y v]} ...)
       (define (swap)
         (let ([tmp (p)])
           (p y)
           (set! y tmp))
         ...)
       (dynamic-wind swap (lambda () e1 e2 ...) swap))])
