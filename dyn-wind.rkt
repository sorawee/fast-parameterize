#lang racket/base

;; Adapted from https://www.scheme.com/csug6/system.html
;;
;; Note that this version doesn't evaluate the body in tail position

(provide make-parameter
         parameterize)

(require syntax/parse/define
         (for-syntax racket/base))

(define make-parameter
  (case-lambda
    [(init guard)
     (let ([v init])
       (case-lambda
         [() v]
         [(u) (set! v (guard u))]
         [(_ u) (set! v u)]))]
    [(init)
     (make-parameter init (lambda (x) x))]))

(define-syntax-parser parameterize
  [(_ () e1 e2 ...) #'(let () e1 e2 ...)]
  [(_ ([x v] ...) e1 e2 ...)
   #:with (p ...) (generate-temporaries #'(x ...))
   #:with (y ...) (generate-temporaries #'(x ...))
   #'(let ({~@ [p x] [y v]} ...)
       (dynamic-wind
         (lambda ()
           (let ([tmp (p)])
             (p y)
             (set! y tmp))
           ...)
         (lambda () e1 e2 ...)
         (lambda () (p #f y) ...)))])
