#lang racket

;; A Program is ([Listof FDef] Expr)

;; An FDef is (define Id Expr)

;; An Expr is one of:
;; - Value
;; - Id (symbol)
;; - (PrimOp Expr Expr)
;; - (PrimOp Expr)
;; - (if Expr Expr Expr)
;; - (lambda (Id) Expr)
;; - (Expr Expr)

;; A Value is one of:
;; - Number
;; - Boolean

;; val? : Any -> Boolean
(define (val? v)
  (or (number? v) (boolean? v)))

;; A PrimOp is one of:
;; - +, *, -, /, zero?, sub1, add1

(define prim-ops `(+ - * / zero? sub1 add1))

;; prim-op? : Any -> Boolean
(define (prim-op? p)
  (memv p prim-ops))

;; sym-to-op : Symbol -> PrimOp
(define (sym-to-op p)
  (match p
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]
    ['zero? zero?]
    ['add1 add1]
    ['sub1 sub1]))

;; An Env is [AssocList Id Expr]

;; lookup : Env Id -> [Maybe Expr]
(define (lookup env e)
  (let ((v (assv e env)))
    (if v (cdr v) v)))

;; extend : Env Id Expr -> Env
(define (extend env x e)
  `((,x . ,e) . ,env))

;; add-defs-to-env : [Listof FDef] -> Env
(define (add-defs-to-env defs)
  (foldr
   (lambda (def env)
     (match def
       [`(define ,id ,expr) (cons `(,id . ,expr) env)]
       [else (error "Not a definition:" def)]))
   '()
   defs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;
;;            Interpreter
;;            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; eval : Program -> Value
(define (eval p)
  (match p
    [`(,defs ,expr) (eval-expr expr (add-defs-to-env defs))]
    [else (error "Not a program:" p)]))


;; eval-expr : Expr Env -> Value
(define (eval-expr expr env)
  (let loop ([e expr]
             [env env])
    (match e
      [(? val?) e]
      [(? prim-op?) (sym-to-op e)]
      [(? symbol?) (eval-expr (lookup env e) env)]
      [`(,(? prim-op? p) ,e1) ((loop p env) (loop e1 env))]
      [`(,(? prim-op? p) ,e1 ,e2) ((loop p env) (loop e1 env) (loop e2 env))]
      [`(if ,p ,t ,f) (if (loop p env) (loop t env) (loop f env))]
      [`(lambda (,x) ,b) (lambda (a) (loop b (extend env x (loop a env))))]
      [`(,rator ,rand) ((loop rator env) (loop rand env))]
      [else (error "Not an expression:" e)])))

(module+ test (require rackunit)
  (check-equal? (eval '(((define add2 (lambda (x) (+ x 2)))) (add2 3))) 5)
  (check-equal? (eval '(((define fact (lambda (x) (if (zero? x) 1 (* x (fact (sub1 x))))))) (fact 5))) 120)
  (check-equal? (eval '(((define even? (lambda (x) (if (zero? x) #t (odd? (sub1 x)))))
                         (define odd? (lambda (x) (if (zero? x) #f (even? (sub1 x))))))
                        (even? 5))) #f)
  (check-equal? (eval '(((define add (lambda (x) (lambda (y) (+ x y)))))
                        ((add 2) 3))) 5))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;
;;            Partial Evaluator
;;            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


;; partial-eval : Program -> Program
(define (partial-eval p)
  (match p
    [`(,defs ,expr) (let ((init-env (add-defs-to-env defs)))
                      `(,(partial-eval-defs defs '()) ,(partial-eval-expr expr init-env)))]
    [else (error "Not a program:" p)]))


;; partial-eval-defs : [Listof FDef] Env -> [Listof FDef]
(define (partial-eval-defs defs env)
  (map (lambda (def)
         (match def
           [`(define ,id ,expr) `(define ,id ,(partial-eval-expr expr env))]
           [else (error "Not a definition:" def)]))
       defs))


;; partial-eval-expr : Expr Env -> Expr
(define (partial-eval-expr expr env)
  (let loop ([e expr]
             [env env])
    (match e
      [(? val?) e]
      [(? symbol?) (let ((expr (lookup env e)))
                     (if expr (partial-eval-expr expr env) e))]
      [`(,(? prim-op? p) ,e1) (let ((e1v (loop e1 env)))
                                (if (val? e1v)
                                    ((sym-to-op p) e1v)
                                    `(,p ,e1v)))]
      [`(,(? prim-op? p) ,e1 ,e2) (let ((e1v (loop e1 env))
                                        (e2v (loop e2 env)))
                                    (if (and (val? e1v) (val? e2v))
                                        ((sym-to-op p) e1v e2v)
                                        `(,p ,e1v ,e2v)))]
      [`(if ,p ,t ,f) (let ((pv (loop p env)))
                        (if (val? pv)
                            (if pv (loop t env) (loop f env))
                            `(if ,pv ,(loop t env) ,(loop f env))))]
      [`(lambda (,x) ,b) `(lambda (,x) ,(loop b env))]
      [`(,rator ,rand) (partial-eval-app e env)]
      [else (error "Not an expression:" e)])))

;; partial-eval-app : Expr Env -> Expr
(define (partial-eval-app expr env)
  (match expr
    [`((lambda (,x) ,b) ,arg)
     (let ((argv (partial-eval-expr arg env)))
       (partial-eval-expr (subst `((,x  . ,argv)) b) env))]
    [`(,f ,arg)
     (let* ((fl (flatten f))
            (fname (car fl))
            (args `(,@(cdr fl) ,arg))
            (fdef (lookup env fname)))
       (if fdef
           (let-values (((vars body) (extract-vars fdef)))
             (partial-eval-expr (subst (map cons vars args) body) env))
           expr))]
    [else (error "Not an application:" expr)]))

;; subst : [AssocList Id Expr] Expr -> Expr
(define (subst mapping e)
  (let loop ([m mapping] [e e])
    (match e
      [(? symbol?) (let ((assoc (assv e m)))
                     (if assoc (cdr assoc) e))]
      [(? val?) e]
      [`(,(? prim-op? p) ,e1 ,e2) `(,p ,(loop m e1) ,(loop m e2))]
      [`(if ,p ,t ,f) `(if ,(loop m p) ,(loop m t) ,(loop m f))]
      [`(lambda (,x2) ,b) (if (memv x2 (map car m))
                              e
                              `(lambda (,x2) ,(loop m b)))]
      [`(,rator ,rand) `(,(loop m rator) ,(loop m rand))])))

;; extract-vars: Expr -> [Listof Id], Expr
;; extracts the variables from a possibly-multi-argument lambda into a list,
;; and returns the body 
(define (extract-vars e)
  (match e
    [`(lambda (,x) ,b) (let-values (((args body) (extract-vars b)))
                         (values (cons x args) body))]
    [else (values '() e)]))

(module+ test
  ;; simple partial evaluation (removing unecessary if statements, arthmetic ops etc)
  (check-equal? (partial-eval '(() 5)) '(() 5))
  (check-equal? (partial-eval '(() (add1 5))) '(() 6))
  (check-equal? (partial-eval '(() (+ 5 10))) '(() 15))
  (check-equal? (partial-eval '(() (- 5 10))) '(() -5))
  (check-equal? (partial-eval '(() (- x 10))) '(() (- x 10)))
  (check-equal? (partial-eval '(() (if #t 4 5))) '(() 4))
  (check-equal? (partial-eval '(() (if #f 4 5))) '(() 5))
  (check-equal? (partial-eval '(() (if (zero? 0) 4 5))) '(() 4))
  (check-equal? (partial-eval '(() (if (zero? 2) 4 5))) '(() 5))
  (check-equal? (partial-eval '(() (if (zero? x) 4 5))) '(() (if (zero? x) 4 5)))

  ;; simple function application
  (check-equal? (partial-eval '(((define add2 (lambda (x) (+ x 2)))) (add2 3)))
                '(((define add2 (lambda (x) (+ x 2)))) 5))
  (check-equal? (partial-eval '(((define fact (lambda (x) (if (zero? x) 1 (* x (fact (sub1 x))))))) (fact 5)))
                '(((define fact (lambda (x) (if (zero? x) 1 (* x (fact (sub1 x))))))) 120))
  (check-equal? (partial-eval '(((define even? (lambda (x) (if (zero? x) #t (odd? (sub1 x)))))
                                 (define odd? (lambda (x) (if (zero? x) #f (even? (sub1 x))))))
                                (even? 5)))
                '(((define even? (lambda (x) (if (zero? x) #t (odd? (sub1 x)))))
                   (define odd? (lambda (x) (if (zero? x) #f (even? (sub1 x))))))
                  #f))
  (check-equal? (partial-eval '(((define add (lambda (x) (lambda (y) (+ x y)))))
                                ((add 2) 3)))
                '(((define add (lambda (x) (lambda (y) (+ x y))))) 5))

  (check-equal? (partial-eval '(((define exp (lambda (x) (lambda (y) (if (zero? y) 1 (* x ((exp x) (sub1 y))))))))
                                ((exp 2) 3)))
                '(((define exp (lambda (x) (lambda (y) (if (zero? y) 1 (* x ((exp x) (sub1 y)))))))) 8))
  (check-equal? (partial-eval '(((define exp (lambda (x) (lambda (y) (if (zero? y) 1 (* x ((exp x) (sub1 y))))))))
                                ((exp x) 3)))
                '(((define exp (lambda (x) (lambda (y) (if (zero? y) 1 (* x ((exp x) (sub1 y)))))))) (* x (* x (* x 1))))))