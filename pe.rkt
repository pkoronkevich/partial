#lang racket

(provide partial-eval)

;; A Linket is (linklet (Imports) (Exports) Def ... Expr)
;; ignoring imports and exports for now

;; An Def is (define-values (Id ...) Expr)
;; of course, with the constraint that Expr produces as many values as Ids

;; An Expr is one of:
;; - Value
;; - Id (symbol)
;; - (PrimOp Expr ...) ;; general builtins, like list
;; - (ConstrOp Expr ...) ;; constructors, like make-struct-type 
;; - (if Expr Expr Expr)
;; - (lambda (Id ...) Expr)
;; - (lambda Id Expr)
;; - (begin Expr ...)
;; - (Expr Expr ...) ;; application

;; A Value is one of:
;; - Number
;; - Boolean
;; - String

;; val? : Any -> Boolean
(define (val? v)
  (or (number? v) (boolean? v) (string? v)))

;; A PrimOp is one of:
;; - +, *, -, /, sub1, add1, <, >, >=, <=
;; - cons, list, car, cdr
;; - string-append
;; - zero?, exact-integer?, procedure?, procedure-arity-includes?, pair?, vector?, =, equal?
;; - vector, vector-ref, vector-length, vector->immutable-vector
;; - raise-argument-error, void

(define prim-ops `(+ - * / sub1 add1 < > >= <=
                     cons list car cdr list-ref for-each
                     string-append
                     exact-integer? zero? procedure? procedure-arity-includes? pair? vector? = equal?
                     vector vector-ref vector-length vector->immutable-vector
                     raise-argument-error void))

;; prim-op? : Any -> Boolean
(define (prim-op? p)
  (memv p prim-ops))
 
;; sym-to-op : Symbol -> PrimOp
(define (sym-to-op p)
  (eval-syntax (datum->syntax #'here p)))

#;(define (sym-to-op p)
  (match p
    ['+ +]
    ['- -]
    ['* *] 
    ['/ /]
    ['> >]
    ['< <]
    ['>= >=]
    ['<= <=]
    ['add1 add1]
    ['sub1 sub1]
    ['cons cons]
    ['list list]
    ['car car]
    ['cdr cdr]
    ['string-append string-append]
    ['= =]
    ['equal? equal?]
    ['zero? zero?]
    ['procedure? procedure?]
    ['procedure-arity-includes? procedure-arity-includes?]
    ['pair? pair?]
    ['vector? vector?]
    ['vector vector]
    ['vector-ref vector-ref]
    ['vector-length vector-length]
    ['vector->immutable-vector vector->immutable-vector]
    ['raise-argument-error raise-argument-error]
    ['void void]))

;; A ConstrOp is one of
;; - values, make-struct-type-property, make-struct-field-accessor

(define constr-ops `(values make-struct-type-property make-struct-field-accessor))

;; constr-op? : Any -> Boolean
(define (constr-op? c)
  (memv c constr-ops))

;; sym-to-constr : Symbol -> ConstrOp
(define (sym-to-constr c)
  (sym-to-op c))

;; An Env is [AssocList Id Expr]

;; lookup : Env Id -> [Maybe Expr]
(define (lookup env e)
  (let ((v (assv e env)))
    (if v (cdr v) v)))

;; extend : Env Id Expr -> Env
(define (extend env x e)
  `((,x . ,e) . ,env))

#|
;; add-defs-to-env : [Listof Def] -> Env
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

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;
;;            Partial Evaluator
;;            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


;; partial-eval : Linklet -> Linklet
(define (partial-eval l)
  (match l
    [`(linklet () () . (,defs ... ,expr))
     (let-values (((p-defs env) (partial-eval-defs defs '())))
       `(linklet () () ,@p-defs ,(partial-eval-expr expr env)))]
    [else (error "Not a proper linklet: " l)]))


;; partial-eval-defs : [Listof Def] Env -> [Listof Def] , Env
(define (partial-eval-defs defs env)
  (cond
    [(empty? defs) (values defs env)]
    [else (match (car defs)
            [`(define-values (,id) ,expr)
             (let* ((p-expr (partial-eval-expr expr env))
                    (ext-env (extend env id p-expr)))
               (let-values (((p-defs new-env) (partial-eval-defs (cdr defs) ext-env)))
                 (values
                  (cons `(define-values (,id) ,p-expr) p-defs)
                  new-env)))]
            [`(define-values (,ids ...) ,expr)
             ;; TODO multiple values?
             (let ((p-expr (partial-eval-expr expr env)))
               (let-values (((p-defs new-env) (partial-eval-defs (cdr defs) env)))
                 (values
                  (cons `(define-values (,@ids) ,p-expr) p-defs)
                  new-env)))])]))
 

;; partial-eval-expr : Expr Env -> Expr
(define (partial-eval-expr expr env)
  (let loop ([e expr]
             [env env])
    (match e
      [(? val?) e]
      [`(quote ,e) `(quote ,e)]
      ['(void) '(void)]
      [(? symbol?)
       (let ((expr (lookup env e)))
         (if expr (partial-eval-expr expr env) e))]
      [`(,(? prim-op? p) ,e ...)
       (let ((es (map (lambda (x) (loop x env)) e)))
         (if (andmap val? es)
             (apply (sym-to-op p) es)
             `(,p ,@es)))]
      ;; for now ignoring constructor ops
      [`(,(? constr-op? c) ,e ...)
       (let ((es (map (lambda (x) (loop x env)) e)))
         `(,c ,@es))]
      [`(if ,p ,t ,f)
       (let ((pv (loop p env)))
         (if (val? pv)
             (if pv (loop t env) (loop f env))
             `(if ,pv ,(loop t env) ,(loop f env))))]
      [`(begin ,e) (loop e env)]
      [`(begin ,es ...) `(begin ,@(map (lambda (e) (loop e env)) es))]
      [`(lambda (,x ...) ,b) `(lambda (,@x) ,(loop b env))]
      [`(lambda ,x ,b) `(lambda ,x ,(loop b env))]
      [`(let-values (,x ...) ,b) `(let-values (,@x) ,(loop b env))]
      [`(,rator ,rand ...) (partial-eval-app e env)]
      [else (error "Not an expression:" e)])))

;; partial-eval-app : Expr Env -> Expr
(define (partial-eval-app expr env)
  (match expr
    [`((lambda (,x ...) ,b) ,args ...)
     (let ((argvs (map (lambda (a) (partial-eval-expr a env)) args)))
       (partial-eval-expr (subst (map cons x argvs) b) env))]
    [`(,f ,args ...)
     (let ((fdef (lookup env f))
           (argvs (map (lambda (a) (partial-eval-expr a env)) args)))
       (if fdef
           (let-values (((vars body) (extract-vars fdef)))
             (partial-eval-expr (subst (map cons vars argvs) body) env))
           `(,f ,@argvs)))]
    [else (error "Not an application:" expr)]))

;; subst : [AssocList Id Expr] Expr -> Expr
(define (subst mapping e)
  (let loop ([m mapping] [e e])
    (match e
      [(? symbol?) (let ((assoc (assv e m)))
                     (if assoc (cdr assoc) e))]
      [(? val?) e]
      [`(,(? prim-op? p) ,es ...) `(,p ,@(map (lambda (e) (loop m e)) es))]
      [`(if ,p ,t ,f) `(if ,(loop m p) ,(loop m t) ,(loop m f))]
      [`(lambda (,x2) ,b) (if (memv x2 (map car m))
                              e
                              `(lambda (,x2) ,(loop m b)))]
      [`(lambda ,x2 ,b) (if (memv x2 (map car m))
                            e
                            `(lambda ,x2 ,(loop m b)))]
      [`(,es ...) (map (lambda (e) (loop m e)) es)])))

;; extract-vars: Expr -> [Listof Id], Expr
;; extracts the variables from a possibly-multi-argument lambda into a list,
;; and returns the body 
(define (extract-vars e)
  (match e
    [`(lambda (,x) ,b) (values `(,x) b)]
    [`(lambda (,x ...) ,b) (values x b)]
    [else (values '() e)]))