#lang racket

(require rackunit "pe.rkt")
(require (for-syntax syntax/parse))

;; ah, the power of abstraction
(define-syntax test-program 
  (syntax-parser #:literals(quote)
    [(_ '(actual ...) '(expected ...))
     #`(check-equal? (partial-eval '(linklet () () #,@#'(actual ...))) '(linklet () () #,@#'(expected ...)))]
    [(_ actual expected)
     #`(check-equal? (partial-eval '(linklet () () #,#'actual)) '(linklet () () #,#'expected))]))

;; simple partial evaluation (removing unecessary if statements, arthmetic ops etc)
(test-program 5 5)
(test-program (add1 5) 6)
(test-program (+ 5 10 4) 19)
(test-program (- 5 10) -5)
(test-program (- x 10) (- x 10))
(test-program (if #t 4 5) 4)
(test-program (if #f 4 5) 5)
(test-program (if (zero? 0) 4 5) 4)
(test-program (if (zero? 2) 4 5) 5)
(test-program (if (zero? x) 4 5) (if (zero? x) 4 5))
(test-program (string-append "hello " "world") "hello world")
(test-program (string-append "hello " x) (string-append "hello " x))

;; simple definitions
(test-program '((define-values (x y z) (values 1 2 3)) (+ x y z))
              '((define-values (x y z) (values 1 2 3)) 6))

(test-program '((define-values (x y z) (values 1 2 3))
                (let-values (((x1 x2 x3) (values 4 5 6)))
                  (+ (+ x y z) (+ x1 x2 x3))))
              '((define-values (x y z) (values 1 2 3)) 21))

#;(test-program '((define-values (name) "paulette")
                (let-values (((hi comma) ((lambda (x) (values "hello" ", ")) 3)))
                  (string-append hi comma name)))
              '((define-values (name) "paulette") "hello, paulette"))

(test-program '((define-values (x y z) (values 1 2 b)) ;; partial instantiation!!
                (let-values (((x1 x2 x3) (values 4 5 6)))
                  (+ (+ x y z) (+ x1 x2 x3))))
              '((define-values (x y z) (values 1 2 b)) (+ (+ 1 2 b) 15)))

(test-program '((define-values (x y z) (values 1 2 b)) ;; partial instantiation...
                (let-values (((x1 x2 x3) (values 4 c 6))) ;; ...twice!
                  (+ (+ x y z) (+ x1 x2 x3))))
              '((define-values (x y z) (values 1 2 b)) (+ (+ 1 2 b) (+ 4 c 6))))

(test-program '((define-values (a b c) (values 10 10 10))
                (define-values (x y z) (values 1 2 b)) 
                (let-values (((x1 x2 x3) (values 4 c 6))) 
                  (+ a (+ x y z) (+ x1 x2 x3))))
              '((define-values (a b c) (values 10 10 10))
                (define-values (x y z) (values 1 2 b))
                43))

;; vector ops
(test-program '((define-values (v) (vector (add1 1) 2 (+ 4 3))) (vector-length v))
              '((define-values (v) (vector 2 2 7)) 3))

(test-program '((define-values (v) (vector (add1 1) 2 (+ 4 3))) (vector? v))
              '((define-values (v) (vector 2 2 7)) #t))

(test-program '((define-values (v) (vector (add1 1) 2 (+ 4 3))) (vector-ref v 2))
              '((define-values (v) (vector 2 2 7)) 7))


;; list ops
(test-program (map add1 (list 1 2 3)) '(2 3 4))
(test-program (map (lambda (x) (add1 x)) (list 1 2 3)) '(2 3 4))
(test-program (map f (list 1 2 3)) '((f 1) (f 2) (f 3)))
(test-program (apply + (list 1 2 3)) 6)
(test-program (apply + '()) 0)
(test-program (apply + ls) (apply + ls))
(test-program (apply values (list 1 2 3)) (values 1 2 3))
(test-program (foldr + 0 (list 1 2 3)) 6)
(test-program (foldr cons '() (list 1 2 3)) '(1 2 3))
(test-program (foldr (lambda (x y) (cons (add1 x) y)) '() (list 1 2 3)) '(2 3 4))
;;; can't reduce since unknown z
(test-program (foldr (lambda (x y) (cons (add1 z) y)) '() (list 1 2 3)) 
              '((add1 z) (add1 z) (add1 z)))


;;; VERY simple function application
(test-program '((define-values (add2) (lambda (x) (+ x 2))) (add2 3))
              '((define-values (add2) (lambda (x) (+ x 2))) 5))

(test-program '((define-values (fact) (lambda (x) (if (zero? x) 1 (* x (fact (sub1 x)))))) (fact 5))
              '((define-values (fact) (lambda (x) (if (zero? x) 1 (* x (fact (sub1 x)))))) 120))

;;;;;;; MUTUAL RECURSION NOT TERMINATING 

#;(test-program '((define-values (even?) (lambda (x) (if (zero? x) #t (odd? (sub1 x)))))
                (define-values (odd?)  (lambda (x) (if (zero? x) #f (even? (sub1 x)))))
                (even? 5))
              '((define-values (even?) (lambda (x) (if (zero? x) #t (odd? (sub1 x)))))
                (define-values (odd?)  (lambda (x) (if (zero? x) #f (if (zero? (sub1 x)) #t (odd? (sub1 (sub1 x)))))))
                #f))

#;(test-program '((define-values (even?) (lambda (x) (if (zero? x) #t (odd? (sub1 x)))))
                (define-values (odd?)  (lambda (x) (if (zero? x) #f (even? (sub1 x)))))
                (even? x))
              '((define-values (even?) (lambda (x) (if (zero? x) #t (odd? (sub1 x)))))
                (define-values (odd?)  (lambda (x) (if (zero? x) #f (if (zero? (sub1 x)) #t (odd? (sub1 (sub1 x)))))))
                (even? x)))

(test-program '((define-values (add) (lambda (x y) (+ x y)))
                (add 2 3))
              '((define-values (add) (lambda (x y) (+ x y))) 5))

(test-program '((define-values (exp) (lambda (x y) (if (zero? y) 1 (* x (exp x (sub1 y))))))
                (exp 2 3))
              '((define-values (exp) (lambda (x y) (if (zero? y) 1 (* x (exp x (sub1 y)))))) 8))

(test-program '((define-values (exp) (lambda (x y) (if (zero? y) 1 (* x (exp x (sub1 y))))))
                (exp x 3))
              '((define-values (exp) (lambda (x y) (if (zero? y) 1 (* x (exp x (sub1 y))))))
                (* x (* x (* x 1)))))

(test-program '((define-values (exp) (lambda (x y) (if (zero? y) 1 (* x (exp x (sub1 y))))))
                (exp a b))
              '((define-values (exp) (lambda (x y) (if (zero? y) 1 (* x (exp x (sub1 y))))))
                (if (zero? b) 1 (* a (exp a (sub1 b))))))

#;(define linklet
  (read (open-input-file "tmp.linklet")))
