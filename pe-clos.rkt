#lang racket

;; A TypeTag is one of
;; - 'nat
;; - 'bool
;; - 'func
;; - 'pair

(define (type-tag? t)
  (memv t '(nat bool func pair)))

;; A ValT is [U Val TypeTag]

;; A Closure is a (closure Id [Listof Id] Expr Env SpecCache)
;; A SpecCache is [Map [Listof ValT] SClosure]
;; A SClosure is a (spec-clos Id [Listof SymVal] Expr)
(struct spec-clos [name args [body #:mutable]] #:transparent)

(struct closure (name ids body env cache) #:transparent)

;; A SymVal is a (symval ValT Expr)
(struct symval [val code] #:transparent)

;; lookup-spec-cache: Closure [Listof SymVal] -> SClosure
;; checks if the arguments have a hit in the specialization cache of a closure
(define (lookup-spec-cache cl args)
  (hash-ref (closure-cache cl)
            (map symval-val args)
            #f))

;; type-of: Expr -> TypeTag
(define (type-of exp)
  (match exp
    [(? number?) 'nat]
    [(? boolean?) 'bool]
    ['`(,e ...) 'pair]
    [`(lambda ,x ,e) 'func]
    [else 'value]))

;; A Stack is a [AssocList Closure [Listof SymVal]]

;; apply-closure: Closure [Listof SymVal] Stack -> Expr
(define (apply-closure cl args stack)
  (cond
    [(lookup-spec-cache cl args) =>
     (λ (sc) (make-residual-call sc args))]
    [(specialize? cl args stack)
     (let-values (((new-args new-stack) (generalize-arguments cl args stack)))
       (let ((sc (lookup-spec-cache cl new-args)))
         (if sc
             (make-residual-call sc new-args)
             (make-residual-call (specialize-closure cl new-args new-stack) args))))]
    [else (primitive-apply-closure cl args stack)]))

;; specialize? : Closure [Listof SymVal] Stack -> Bool
(define (specialize? cl args stack)
  (cond
    [(null? stack) #f]
    [(eqv? (closure-name (caar stack)) (closure-name cl)) #t]
    [(eqv? (car stack) 'conditional-marker) (specialize? cl args (cdr stack))]
    [else (specialize? cl args (cdr stack))]))

;; generalize-arguments: Closure [Listof SymVal] Stack -> [Listof SymVal] , Stack
(define (generalize-arguments cl args stack)
  (define-values (stack-args new-stack) (get-args cl args stack))
  (define abs-args (map abstract-args stack-args args))
  (values abs-args (cons (cons cl abs-args) new-stack)))

;; get-closures: Closure [Listof SymVal] Stack -> [Listof SymVal], Stack
;; returns the arguments most similar to the current call
;; wtf is most similar??? currently just returns first one
(define (get-args cl args stack)
  (cond
    [(null? stack) (values args stack)]
    [(eqv? (closure-name cl) (closure-name (caar stack))) (values (caadr stack) (cdr stack))]
    [else
     (let-values (((new-args new-stack) (get-args cl args (cdr stack))))
       (values new-args (cons (car stack) new-stack)))]))

;; abstract-types: SymVal SymVal -> SymVal
(define (abstract-args s1 s2)
  (define lub (least-upper-bound (symval-val s1) (symval-val s2)))
  (if (type-tag? lub) (symval lub (gensym)) (symval lub lub)))


;; least-upper-bound: ValT ValT -> ValT
(define (least-upper-bound v1 v2)
  (match `(,v1 ,v2)
    [`(,(? number?) ,(? number?)) 'nat]
    [`(,(? boolean?) ,(? boolean?)) 'bool]
    [`(,(? list?) ,(? list?)) 'pair]
    [else 'value]))

;; specialize-closure: Closure [Listof SymVal] Stack -> Closure
(define (specialize-closure cl args stack)
  (let ((sc (spec-clos (gensym) args "to be replaced")))
    (hash-set! (closure-cache cl) (map symval-val args) sc)
    (set-spec-clos-body! sc (primitive-apply-closure cl args stack))))

;; primitive-apply-closure: Closure [Listof SymVal] Stack -> Expr
(define (primitive-apply-closure cl args stack)
  (let* ((ids (closure-ids cl))
         (body (closure-body cl))
         (env (closure-env cl)))
    (partial-eval-expr body
                       (combine-env (make-env (map cons ids args)) env)
                       (cons (cons cl args) stack))))

(define (partial-eval-expr b env stack) #f)
(define (combine-env env stack) #f)
(define (make-env env) #f)

;; make-residual-call: SClosure [Listof SymVal] -> Expr
(define (make-residual-call sc args)
  `(,(spec-clos-name sc) ,@(map symval-code args)))
