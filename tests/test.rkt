#lang racket/base

(define x 15)
(define (! n) (if (zero? n) 1 (* n (! (- n 1)))))
(! x)

