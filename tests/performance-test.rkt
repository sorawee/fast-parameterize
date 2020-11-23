#lang racket/base

;; tests from @mflatt

(require (prefix-in m: fast-parameterize))

(define N 10000000)

(define p (make-parameter 'p))
(define p2 (m:make-parameter 'p2))

'get:native
(time
 (parameterize ([p 'val])
   (for/fold ([v #f]) ([i (in-range N)])
     (p))))

'get:markparam
(time
 (m:parameterize ([p2 'val])
                 (for/fold ([v #f]) ([i (in-range N)])
                   (p2))))

(define unknown void)
(set! unknown unknown)

'set:baseline
(time
 (for/fold ([v #f]) ([i (in-range N)])
   (unknown)))

'set:native
(time
 (for/fold ([v #f]) ([i (in-range N)])
   (parameterize ([p 'val])
     (unknown))))

'set:markparam
(time
 (for/fold ([v #f]) ([i (in-range N)])
   (m:parameterize ([p2 'val])
                   (unknown))))
