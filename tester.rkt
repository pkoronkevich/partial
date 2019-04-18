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
;; not partially evaluating unknowns
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
(test-program (f x) (f x))
(test-program (f 10) (f 10))
(test-program (+ 1 2 3 (f 10)) (+ 1 2 3 (f 10)))

;; simple definitions
(test-program '((define-values (x y z) (values 1 2 3)) (+ x y z))
              '((define-values (x y z) (values 1 2 3)) 6))

(test-program '((define-values (x y z) (values 1 2 3))
                (let-values (((x1 x2 x3) (values 4 5 6)))
                  (+ (+ x y z) (+ x1 x2 x3))))
              '((define-values (x y z) (values 1 2 3)) 21))

(test-program '((define-values (name) "paulette")
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

(test-program '((define-values (v) (vector (add1 1) 2 (+ 4 3))) (vector-length v1))
              '((define-values (v) (vector 2 2 7)) (vector-length v1)))

(test-program '((define-values (v) (vector (add1 1) 2 (+ 4 3))) (vector? v1))
              '((define-values (v) (vector 2 2 7)) (vector? v1)))

(test-program '((define-values (v) (vector (add1 1) 2 (+ 4 3))) (vector-ref v1 2))
              '((define-values (v) (vector 2 2 7)) (vector-ref v1 2)))

(test-program '((define-values (v) (vector (add1 1) 2 (+ 4 3))) (vector-ref v n))
              '((define-values (v) (vector 2 2 7)) (vector-ref (vector 2 2 7) n)))


;; list ops
(test-program (map add1 (list 1 2 3)) '(2 3 4))
(test-program (map (lambda (x) (add1 x)) (list 1 2 3)) '(2 3 4))
(test-program (map f (list 1 2 3)) (cons (f 1) (cons (f 2) (cons (f 3) '()))))
(test-program (apply + (list 1 2 3)) 6)
(test-program (apply + '()) 0)
(test-program (apply + ls) (apply + ls))
(test-program (apply values (list 1 2 3)) (values 1 2 3))
(test-program (foldr + 0 (list 1 2 3)) 6)
(test-program (foldr cons '() (list 1 2 3)) '(1 2 3))
(test-program (foldr (lambda (x y) (cons (add1 x) y)) '() (list 1 2 3)) '(2 3 4))
;; can't reduce since unknown z
(test-program (foldr (lambda (x y) (cons (add1 z) y)) '() (list 1 2 3)) 
              (cons (add1 z) (cons (add1 z) (cons (add1 z) '()))))


;; VERY simple function application
(test-program '((define-values (add2) (lambda (x) (+ x 2))) (add2 3))
              '((define-values (add2) (lambda (x) (+ x 2))) 5))

(test-program '((define-values (fact) (lambda (x) (if (zero? x) 1 (* x (fact (sub1 x)))))) (fact 5))
              '((define-values (fact) (lambda (x) (if (zero? x) 1 (* x (fact (sub1 x)))))) 120))

(test-program '((define-values (intersperse)
                  (lambda (str1 los)
                    (if (null? los) str1 (string-append str1 (car los) (intersperse str1 (cdr los))))))
                (intersperse x y))
              '((define-values (intersperse) (lambda (str1 los) (if (null? los) str1 (string-append str1 (car los) (intersperse str1 (cdr los))))))
                (if (null? y) x (string-append x (car y) (intersperse x (cdr y))))))

(test-program '((define-values (intersperse)
                  (lambda (str1 los)
                    (if (null? los) str1 (string-append str1 (car los) (intersperse str1 (cdr los))))))
                (intersperse "hi" y))
              '((define-values (intersperse) (lambda (str1 los) (if (null? los) str1 (string-append str1 (car los) (intersperse str1 (cdr los))))))
                (if (null? y) "hi" (string-append "hi" (car y) (intersperse "hi" (cdr y))))))

(test-program '((define-values (intersperse)
                  (lambda (str1 los)
                    (if (null? los) str1 (string-append str1 (car los) (intersperse str1 (cdr los))))))
                (intersperse "hi" '("a" "b" "c")))
              '((define-values (intersperse) (lambda (str1 los) (if (null? los) str1 (string-append str1 (car los) (intersperse str1 (cdr los))))))
                "hiahibhichi"))

;; weird "counting up" version of fact
(test-program '((define-values (limit) 5)
                (define-values (fact) (lambda (x) (if (> x limit) 1 (* x  (fact (add1 x))))))
                (fact 1))
              '((define-values (limit) 5)
                (define-values (fact) (lambda (x) (if (> x 5) 1 (* x  (fact (add1 x))))))
                120))

(test-program '((define-values (limit) 5)
                (define-values (fact) (lambda (x) (if (> x limit) 1 (* x  (fact (add1 x))))))
                (fact x))
              '((define-values (limit) 5)
                (define-values (fact) (lambda (x) (if (> x 5) 1 (* x  (fact (add1 x))))))
                (if (> x 5) 1 (* x  (fact (add1 x))))))

;; mutual recursion
(test-program '((define-values (even?) (lambda (x) (if (zero? x) #t (odd? (sub1 x)))))
                (define-values (odd?)  (lambda (x) (if (zero? x) #f (even? (sub1 x)))))
                (even? 5))
              '((define-values (even?) (lambda (x) (if (zero? x) #t (odd? (sub1 x)))))
                (define-values (odd?)  (lambda (x) (if (zero? x) #f (even? (sub1 x)))))
                #f))

(test-program '((define-values (even?) (lambda (x) (if (zero? x) #t (odd? (sub1 x)))))
                (define-values (odd?)  (lambda (x) (if (zero? x) #f (even? (sub1 x)))))
                (even? x))
              '((define-values (even?) (lambda (x) (if (zero? x) #t (odd? (sub1 x)))))
                (define-values (odd?)  (lambda (x) (if (zero? x) #f (even? (sub1 x)))))
                (if (zero? x) #t (odd? (sub1 x)))))

(test-program '((define-values (add) (lambda (x y) (+ x y)))
                (add 2 3))
              '((define-values (add) (lambda (x y) (+ x y))) 5))

(test-program '((define-values (exp) (lambda (x y) (if (zero? y) 1 (* x (exp x (sub1 y))))))
                (exp 2 3))
              '((define-values (exp) (lambda (x y) (if (zero? y) 1 (* x (exp x (sub1 y)))))) 8))

;; unrolling based on one known argument
(test-program '((define-values (exp) (lambda (x y) (if (zero? y) 1 (* x (exp x (sub1 y))))))
                (exp x 3))
              '((define-values (exp) (lambda (x y) (if (zero? y) 1 (* x (exp x (sub1 y))))))
                (* x (* x (* x 1)))))

(test-program '((define-values (exp) (lambda (x y) (if (zero? y) 1 (* x (exp x (sub1 y))))))
                (exp a b))
              '((define-values (exp) (lambda (x y) (if (zero? y) 1 (* x (exp x (sub1 y))))))
                (if (zero? b) 1 (* a (exp a (sub1 b))))))

;; testing n-arg lambdas
(test-program '((define-values (f) (lambda ls (cons 'top ls)))
                (f 'middle 'bottom))
              '((define-values (f) (lambda ls (cons 'top ls)))
                '(top middle bottom)))

(test-program '((define-values (list-by-another-name) (lambda ls ls))
                (list-by-another-name 1 2 3 4))
              '((define-values (list-by-another-name) (lambda ls ls))
                '(1 2 3 4)))

(test-program '((define-values (map-hi)
                  (lambda ls (map (lambda (s) (string-append "Hi, " s)) ls)))
                (map-hi "Sam" "Paulette" "Turab"))
              '((define-values  (map-hi)
                  (lambda ls (map (lambda (s) (string-append "Hi, " s)) ls)))
                '("Hi, Sam" "Hi, Paulette" "Hi, Turab")))

(test-program '((define-values (sum-pos)
                  (lambda ls (foldr (lambda (n res) (if (>= n 0) (+ n res) res)) 0 ls)))
                (sum-pos 3 9 -2 -4 -5 8 0))
              '((define-values (sum-pos)
                  (lambda ls (foldr (lambda (n res) (if (>= n 0) (+ n res) res)) 0 ls)))
                20))

#;(define linklet
  (read (open-input-file "tmp.linklet")))
