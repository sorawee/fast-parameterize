#lang racket/base

(provide parameterize make-parameter)
(require syntax/parse/define
         (for-syntax racket/base))

(define %param-key (vector 'param-key))
(define %param-convert (vector 'param-convert))

(define make-parameter
  (case-lambda
    [(init) (make-parameter init values)]
    [(init converter)
     (define key (vector 'key))
     (define value (converter init))
     (case-lambda
       [()
        (define boxed-value (continuation-mark-set-first #f key))
        (if boxed-value (vector-ref boxed-value 0) value)]
       [(v)
        (define boxed-value (continuation-mark-set-first #f key))
        (if boxed-value
            (vector-set! boxed-value 0 (converter v))
            (set! value (converter v)))
        ]
       [(_ secret)
        (cond
          [(eq? %param-convert secret) converter]
          [(eq? %param-key secret) key])])]))

(define-syntax-parser with-continuation-mark*
  [(_ () body ...+) #'(let () body ...)]
  [(_ ([x y] other ...) body ...+)
   #'(with-continuation-mark x y
       (with-continuation-mark* (other ...) body ...))])

(define-syntax-parser parameterize
  [(_ () body ...+) #'(let () body ...)]
  [(_ ([param init] ...) body ...+)
   #:with (p ...) (generate-temporaries #'(param ...))
   #:with (x ...) (generate-temporaries #'(init ...))
   #:with (y ...) (generate-temporaries #'(init ...))
   #'(let ({~@ [p param] [x init]} ...)
       (let ([y ((p #f %param-convert) x)] ...)
         (with-continuation-mark* ([(p #f %param-key) (vector y)] ...)
           body ...)))])
