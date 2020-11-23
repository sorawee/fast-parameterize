#lang racket/base

;; Adapted from https://srfi.schemers.org/srfi-157/srfi-157.html
;; with an idea to bump the parameterization object from
;; the native parameter and @yjqww6

(provide parameterize make-parameter)
(require syntax/parse/define
         (only-in racket/unsafe/ops unsafe-unbox* unsafe-set-box*!)
         (for-syntax racket/base))

(define parameterization-key (gensym 'parameterization))

(define (current-parameterization)
  (continuation-mark-set-first #f parameterization-key))

(define make-parameter
  (case-lambda
    [(init) (make-parameter init values)]
    [(init converter)
     (define key (gensym 'key))
     (define value init)
     (case-lambda
       [()
        (define ht (current-parameterization))
        (define boxed-value (and ht (hash-ref ht key #f)))
        (if boxed-value (unsafe-unbox* boxed-value) value)]
       [(v)
        (define ht (current-parameterization))
        (define boxed-value (and ht (hash-ref ht key #f)))
        (if boxed-value
            (unsafe-set-box*! boxed-value (converter v))
            (set! value (converter v)))]
       [(ht v) (hash-set ht key (box v))])]))

(define (extend-parameterization xs)
  (for/fold ([ht (or (current-parameterization) (hasheq))])
            ([x (in-list xs)])
    ((car x) ht (cdr x))))

(define-syntax-parser parameterize
  [(_ () body ...+) #'(let () body ...)]
  [(_ ([param init] ...) body ...+)
   #:with (p ...) (generate-temporaries #'(param ...))
   #:with (x ...) (generate-temporaries #'(init ...))
   #'(let ({~@ [p param] [x init]} ...)
       (with-continuation-mark
         parameterization-key
         (extend-parameterization (list (cons p x) ...))
         (let () body ...)))])
