#lang racket

(provide partial-eval)
(require racket/hash racket/trace)

;; A Linket is (linklet (Imports) (Exports) Def ... Expr)
;; ignoring imports and exports for now

;; An Def is (define-values (Id ...) Expr)
;; of course, with the constraint that Expr produces as many values as Ids

;; An Expr is one of:
;; - CodeValue
;; - Id (symbol)
;; - (PrimOp Expr ...) ;; general builtins, like list
;; - (EffectOp Expr ...) ;; effectful operators, like make-struct-type 
;; - (if Expr Expr Expr)
;; - (lambda (Id ...) Expr)
;; - (lambda Id Expr)
;; - (begin Expr ...)
;; - (Expr Expr ...) ;; application

;; A CodeValue is one of:
;; - Number
;; - Boolean
;; - String
;; - ''(CodeValue ...)
;; - '(list CodeValue ...)
;; - '(values CodeValue ...)
;; - '(quote CodeValue)
;; - '(lambda Id CodeValue) -- where Id bound is the only Id in the body

;; cval? : Any -> Boolean
(define (cval? v)
  (match v
    [(or (? number?) (? boolean?) (? string?)) #t] ;; self-quoting
    [`',s #t] ;; symbol
    [`'(,e ...) (andmap cval? e)]
    [`(,op ,e ...)
     #:when (or (eqv? op 'list) (eqv? op 'values) (eqv? op 'quote) (eqv? op 'vector))
     (andmap cval? e)]
    ;[`(lambda () ,b) (cval? b)]
    ;[`(lambda (,xs ...) ,b) (only-free-ids? xs b)]
    ;[`(lambda ,x ,b) (only-free-ids? `(,x) b)]
    [else #f]))

(module+ test (require rackunit)
  (check-equal? (cval? '3) #t)
  (check-equal? (cval? '#t) #t)
  (check-equal? (cval? '"hello") #t)
  (check-equal? (cval? 'x) #f) ;; variable
  (check-equal? (cval? ''a) #t) ;; symbol
  (check-equal? (cval? ''b) #t) ;; symbol
  (check-equal? (cval? ''(1 2 3)) #t)
  (check-equal? (cval? '(list 1 2 3)) #t)
  (check-equal? (cval? '(quote 1)) #t)
  (check-equal? (cval? '(values 1 2 3)) #t)
  ;(check-equal? (cval? '(lambda () x)) #f) ;; x unbound
  ;(check-equal? (cval? '(lambda () 3)) #t)
  ;(check-equal? (not (false? (cval? '(lambda (x) x)))) #t)
  ;(check-equal? (not (false? (cval? '(lambda (x y) x)))) #t)
  ;(check-equal? (cval? '(lambda (a b c) x)) #f) ;; x unbound
  ;(check-equal? (not (false? (cval? '(lambda (x y) (+ x y))))) #t)
  ;(check-equal? (not (false? (cval? '(lambda x x)))) #t)
  ;(check-equal? (cval? '(lambda x (append x y))) #f)
  (check-equal? (cval? '(+ x y)) #f))

;; only-free-ids? : [Listof Id] Expr -> Boolean
;; checks if the given list of Id is the only free identifiers in the expression
(define (only-free-ids? xs b)
  (match b
    [(? cval?) #t]
    [(? symbol?) (or
                  (memv b xs)
                  (effect-op? b)
                  (prim-op? b))]
    [`(if ,p ,t ,f) (and (only-free-ids? xs p) (only-free-ids? xs t) (only-free-ids? xs f))]
    [`(let-values (((,ys ...) ,e)) ,b) (and (only-free-ids? xs e)
                                          (only-free-ids? `(,@ys . ,xs) b))]
    [`(lambda () ,b) (only-free-ids? xs b)]
    [`(lambda (,ys ...) ,b) (only-free-ids? `(,@ys . ,xs) b)]
    [`(lambda ,y ,b) (only-free-ids? `(,y . ,xs) b)]
    [`(,op ,es ...) (andmap (lambda (e) (only-free-ids? xs e)) b)]))

(module+ test 
  (check-equal? (not (false? (only-free-ids? '(x y z) 'x))) #t)
  (check-equal? (not (false? (only-free-ids? '(x y z) '3))) #t)
  (check-equal? (not (false? (only-free-ids? '(x y z) '"hi"))) #t)
  (check-equal? (not (false? (only-free-ids? '(x y z) '(+ x y z)))) #t)
  (check-equal? (not (false? (only-free-ids? '(x y z) ''(x y z)))) #t)
  (check-equal? (not (false? (only-free-ids? '() ''(x y z)))) #t)
  (check-equal? (not (false? (only-free-ids? '(x y z) '(if x y z)))) #t)
  (check-equal? (not (false? (only-free-ids? '(x y z) '(if 'hi y z)))) #t)
  (check-equal? (not (false? (only-free-ids? '(x y z) '(if (null? x) y (+ z z))))) #t)
  (check-equal? (not (false? (only-free-ids? '(x y z) '(let-values (((a b c) (values x y z))) (+ a b c x y z))))) #t)
  (check-equal? (not (false? (only-free-ids? '(x y z) '(let-values (((a b c) (values 1 2 3))) (+ a b c))))) #t)
  (check-equal? (not (false? (only-free-ids? '(x y z) '(let-values (((a b c) (values 1 2 3))) (if (zero? x) (+ b y) (- c z)))))) #t)
  (check-equal? (not (false? (only-free-ids? '(x y z) '(let-values (((h) x))
                                                         (let-values (((a b c) (values 1 2 3)))
                                                           (if (zero? x) (+ b y) (if (null? h) (+ a x) (- c z)))))))) #t)
  (check-equal? (not (false? (only-free-ids? '(x y z) '(lambda () x)))) #t)
  (check-equal? (not (false? (only-free-ids? '(x y z) '(lambda () (+ x y z))))) #t)
  (check-equal? (not (false? (only-free-ids? '(x y z) '(lambda () (lambda (c) (+ x y z)))))) #t)
  (check-equal? (not (false? (only-free-ids? '(x y z) '(lambda () (lambda (c) (if c (+ x y z) (- x y z))))))) #t)
  (check-equal? (not (false? (only-free-ids? '(x y z) '(lambda () (lambda (c b a) (if (zero? (+ c b a)) (+ x y z) (- x y z))))))) #t)
  (check-equal? (not (false? (only-free-ids? '(x y z) '(lambda ()
                                                         (lambda (c b a)
                                                           (lambda ls
                                                             (cons (if (zero? (+ c b a)) (+ x y z) (- x y z)) ls))))))) #t)
  (check-equal? (only-free-ids? '(y z) 'x) #f)
  (check-equal? (only-free-ids? '(a b c) '(+ x y z)) #f)
  (check-equal? (only-free-ids? '(x) '(if x y z)) #f)
  (check-equal? (only-free-ids? '(x y) '(if 'hi y z)) #f)
  (check-equal? (only-free-ids? '(x b z) '(if (null? x) y (+ z z))) #f)
  (check-equal? (only-free-ids? '(z) '(let-values (((a b c) (values x y z))) (+ a b c x y z))) #f)
  (check-equal? (only-free-ids? '(x y z) '(let-values (((a b c) (values 1 2 3))) (+ r t y))) #f)
  (check-equal? (only-free-ids? '(x z) '(let-values (((a b c) (values 1 2 3))) (if (zero? x) (+ b y) (- c z)))) #f)
  (check-equal? (only-free-ids? '(y z) '(let-values (((h) x))
                                            (let-values (((a b c) (values 1 2 3)))
                                              (if (zero? x) (+ b y) (if (null? h) (+ a x) (- c z)))))) #f)
  (check-equal? (only-free-ids? '(y z) '(lambda () x)) #f)
  (check-equal? (only-free-ids? '(z) '(lambda () (+ x y z))) #f)
  (check-equal? (only-free-ids? '(x y) '(lambda () (lambda (c) (+ x y z)))) #f)
  (check-equal? (only-free-ids? '() '(lambda () (lambda (c) (if c (+ x y z) (- x y z))))) #f)
  (check-equal? (only-free-ids? '(y z) '(lambda () (lambda (c b a) (if (zero? (+ c b a)) (+ x y z) (- x y z))))) #f)
  (check-equal? (only-free-ids? '(x z) '(lambda ()
                                            (lambda (c b a)
                                              (lambda ls
                                                (cons (if (zero? (+ c b a)) (+ x y z) (- x y z)) ls))))) #f))

;; A Value is the equivalent of CodeValue, but Racket's equivalent
;; i.e. ''(2 3) = '(2 3), '(list 1 2) = (list 1 2), etc
(define (val? v)
  (or (list? v) (vector? v)
      (number? v) (boolean? v)
      (string? v)))

;; val->cval: Value -> CodeValue
;; converts Values to CodeValues (s expressions)
(define (val->cval v)
  (match v
    [(or (? number?) (? boolean?) (? string?)) v] ;; self-quoting
    [(or (? list?) (? symbol?)) `',v]
    [(? vector?) `(vector ,@(vector->list v))]
    [else v]))

(module+ test
  (check-equal? (val->cval 3) 3)
  (check-equal? (val->cval #t) #t)
  (check-equal? (val->cval "hi sam") "hi sam")
  (check-equal? (val->cval '()) ''())
  (check-equal? (val->cval '(1 2 3)) ''(1 2 3))
  (check-equal? (val->cval 'a) ''a)
  (check-equal? (val->cval 'c) ''c)
  (check-equal? (val->cval (vector 1 2 3)) '(vector 1 2 3)))


;; cval->val: CodeValue -> [Maybe Value]
;; converts CodeValues (s expressions) to Values (Racket's values)
(define (cval->val v)
  (match v
    [(or (? number?) (? boolean?) (? string?)) v] ;; self-quoting
    [`',e `,e]
    [`'(,e ...) `(,@e)]
    [`(list ,e ...) `(,@e)]
    [`(quote ,e) (quote e)]
    [`(vector ,e ...) (apply vector e)]
    [else v]))


(module+ test
  (check-equal? (cval->val '3) 3)
  (check-equal? (cval->val '#t) #t)
  (check-equal? (cval->val '"hi sam") "hi sam")
  (check-equal? (cval->val ''()) '())
  (check-equal? (cval->val ''(1 2 3)) '(1 2 3))
  (check-equal? (cval->val ''a) 'a)
  (check-equal? (cval->val ''c) 'c)
  (check-equal? (cval->val '(vector 1 2 3)) (vector 1 2 3)))

;; A PrimOp is one of:
;; - +, *, -, /, sub1, add1, <, >, >=, <=
;; - cons, list, car, cdr, list-ref, null?
;; - string-append
;; - zero?, exact-integer?, procedure?, procedure-arity-includes?, pair?, vector?, =, equal?

;; make-ops: makes a hashmap out of the given functions
;; with the key being the symbol of the function
(define-syntax-rule (make-ops i ...)
  (apply hash (append-map list (list 'i ...) (list i ...))))

(define prim-ops (make-ops
                  + - * / sub1 add1 < > >= <=
                  cons car cdr list-ref
                  string-append null?
                  exact-integer? zero? procedure? procedure-arity-includes? pair? = equal?
                  ))

;; prim-op? : Any -> Boolean
(define (prim-op? p)
  (hash-ref prim-ops p #f))

;; get-prim-op : Symbol -> PrimOp
(define (get-prim-op p)
  (hash-ref prim-ops p))

;; A EffectOp is one of
;; - values, make-struct-type, make-struct-type-property, make-struct-field-accessor, current-print

(define effect-ops
  (make-ops values
            make-struct-type-property make-struct-type make-struct-field-accessor
            current-print))

;; effect-op? : Any -> Boolean
(define (effect-op? c)
  (hash-ref effect-ops c #f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;
;;            Symbolic Values
;;            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A TypeTag is one of
;; - 'nat
;; - 'bool
;; - 'func
;; - 'pair

(define (type-tag? t)
  (memv t '(nat bool func pair value)))

;; A ValT is a [U TypeTag Value]

;; A SymVal is a (sv ValT Expr)
(struct sv [val exp] #:transparent)

;; to-sv: Expr -> SymVal
;; changes an expression to symval
;; if its already a value, places the value version in the value position
;; otherwise, uses get-tt to detect the "type"
(define (to-sv e)
  (if (cval? e)
      (sv (cval->val e) e)
      (sv (get-tt e) e)))

;; split prim-ops into groups based on the type returned

(define nat-ops
  (make-ops + - * / sub1 add1 < > >= <=))

;; nat-op? : Any -> Boolean
(define (nat-op? p)
  (hash-ref nat-ops p #f))

(define bool-ops
  (make-ops null? exact-integer? zero? procedure?
            procedure-arity-includes? pair? = equal?))

;; bool-op? : Any -> Boolean
(define (bool-op? p)
  (hash-ref bool-ops p #f))

(define pair-ops
  (make-ops null? exact-integer? zero? procedure?
            procedure-arity-includes? pair? = equal?))

;; pair-op? : Any -> Boolean
(define (pair-op? p)
  (hash-ref pair-ops p #f))


;; get-tt: Expr -> TypeTag
(define (get-tt e)
  (match e
    [`(,(? nat-op?) ,es ...) 'nat]
    [`(,(? bool-op?) ,es ...) 'bool]
    [`(,(? pair-op?) ,es ...) 'pair]
    [`(lambda ,es ...) 'func]
    [(? prim-op?) 'func]
    [else 'value]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;
;;            Environments
;;            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An Env is [ImmutableHash Id SymVal]


;; make-env : [AssocList Id SymVal] -> Env
(define (make-env al)
  (make-immutable-hash al))

;; lookup : Env Id -> [Maybe SymVal]
(define (lookup env x)
  (hash-ref env x #f))

;; lookup-val : Env Id -> [Maybe Val]
(define (lookup-val env x)
  (let ((e (hash-ref env x #f)))
    (if e
        (let ((v (sv-val e)))
          (if (type-tag? v) #f v))
        e)))

;; lookup-tag : Env Id -> [Maybe TypeTag]
(define (lookup-tag env x)
  (let ((e (hash-ref env x #f)))
    (if e
        (let ((t (sv-val e)))
          (if (type-tag? t) t #f))
        e)))

;; lookup-expr : Env Id -> [Maybe Expr]
(define (lookup-expr env x)
  (let ((e (hash-ref env x #f)))
    (if e (sv-exp e) e)))

;; extend : Env Id SymVal -> Env
(define (extend env x e)
  (hash-set env x e))

;; combine-env : Env Env -> Env
(define (combine-env env1 env2)
  (hash-union env1 env2 #:combine/key (lambda (k v1 v2) v1)))

;; remove-ids : Env [U Id [Listof Id]] -> Env
(define (remove-ids env ids)
  (cond
    [(null? ids) env]
    [(symbol? ids) (hash-remove env ids)]
    [else (for/hash ([(k v) env] #:unless (memv k ids))
            (values k v))]))

;; in-env? : Env Id -> Boolean
(define (in-env? env id)
  (lookup env id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;
;;            Partial Evaluator
;;            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; partial-eval : Linklet -> Linklet
(define (partial-eval l)
  (match l
    [`(linklet () () . (,defs ... ,expr))
     (let-values (((p-defs env) (partial-eval-defs defs (make-env '()))))
       `(linklet () () ,@p-defs ,(partial-eval-expr expr env empty-stack)))]
    [else (error "Not a proper linklet: " l)]))


;; partial-eval-defs : [Listof Def] Env-> [Listof Def] , Env 
;; returns multiple values since each def needs reference to previous partially evaluated defs
(define (partial-eval-defs defs env)
  (cond
    [(empty? defs) (values defs env)]
    [else (match (car defs)
            [`(define-values (,id) ,expr)
             (let* ((ids (formals expr))
                    (closed-env (remove-ids env ids))
                    (p-expr (partial-eval-expr expr closed-env empty-stack))
                    (ext-env (extend env id (to-sv p-expr))))
               (let-values (((p-defs final-env) (partial-eval-defs (cdr defs) ext-env)))
                 (values
                  (cons `(define-values (,id) ,p-expr) p-defs)
                  final-env)))]
            [`(define-values (,ids ...) ,expr)
             ;; TODO multiple values
             ;; for now, if it is a values expression, add to variables to env
             ;; TODO need to remove ids from env?
             (let ((p-expr (partial-eval-expr expr env empty-stack)))
               (let-values (((p-defs new-env) (partial-eval-defs (cdr defs) env)))
                 (if (values-expr? p-expr)
                     ;; simple value case
                     (let* ((vals (values-args p-expr))
                            (ext-env (make-env (map cons ids vals)))) ;; map the ids to indvl values 
                       (values
                        (cons `(define-values (,@ids) ,p-expr) p-defs)
                        (combine-env ext-env new-env)))
                     ;; otherwise
                     (values
                      (cons `(define-values (,@ids) ,p-expr) p-defs)
                      new-env))))])]))
 

;; partial-eval-expr : Expr Env Stack -> Expr
;; the stack is used for termination (see [Closures] section)
(define (partial-eval-expr expr env stack)
  (let loop ([e expr]
             [env env]
             [s stack])
    (match e
      ;; the basic primitives
      
      [(? cval?) e]
      ;; TODO val case
      ;[(? val?) (val->cval e)]
      [`(quote ,e) `(quote ,e)]
      ['(void) '(void)]
      ;; symbol case
      [(? symbol?)
       (let ((expr (lookup-expr env e)))
         (if (or (eqv? expr e) (not expr)) e (loop expr env s)))]
      
      ;; primitive operations
      ;; cons is special in that you can just collapse it into a list or dotted pair
      ;; UNLESS THE ARGUMENTS ARE IDENTIFIERS (symbols)
      [`(cons ,a ,b)
       (if (or (symbol? a) (symbol? b))
           (if (or (not (in-env? env a))
                   (not (in-env? env b)))
               `(cons ,(loop a env s) ,(loop b env s))
               (loop `(cons ,(loop a env s) ,(loop b env s)) env s))
           (val->cval
            `(,(cval->val (loop a env s)) . ,(cval->val (loop b env s)))))]
      
      ;; can replace with result if all arguments are known
      ;; TODO: what if arguments are effectful, i.e. read?
      [`(,(? prim-op? p) ,e ...)
       (let ((es (map (lambda (x) (loop x env s)) e)))
         (if (andmap cval? es)
             (let* ((op (get-prim-op p))
                    (vs (map cval->val es))
                    (res (apply op vs)))
              (val->cval res)) 
             `(,p ,@es)))]
      ;; for now ignoring "effectful" (struct) ops
      [`(,(? effect-op? c) ,e ...)
       (let ((es (map (lambda (x) (loop x env s)) e)))
         `(,c ,@es))]
      ;; if
      [`(if ,p ,t ,f)
       (let ((pv (loop p env s)))
         (if (cval? pv)
             (if (cval->val pv) (loop t env s) (loop f env s))
             `(if ,pv ,(loop t env (mark-stack s)) ,(loop f env (mark-stack s)))))]
      ;; (begin e) == e
      [`(begin ,e) (loop e env s)]
      [`(begin ,es ...) `(begin ,@(map (lambda (e) (loop e env s)) es))]
      ;; lambda case
      [`(lambda ,x ,b) `(lambda ,x ,(loop b env s))]

      ;; vectors
      
      [(? vector-expr?) (map (lambda (e) (loop e env s)) e)]
      [`(vector? ,v)
       (cond
         [(symbol? v)
          (let ((val (lookup-val env v)))
            (if val
                (vector? val)
                `(vector? ,v)))]
         [(vector-expr? v) #t]
         [else `(vector? ,(loop v env s))])]
      [`(vector-length ,v)
       (cond
         [(symbol? v)
          (let ((val (lookup-val env v)))
            (if val
                (vector-length val)
                `(vector-length ,v)))]
         [(vector-expr? v) (sub1 (length v))]
         [else `(vector-length ,(loop v env s))])]
      [`(vector-ref ,v ,i)
       (let ((pi (loop i env s)))
           (cond
             [(symbol? v)
              (let ((val (lookup-val env v)))
                (if val
                    (loop (vector-ref val pi) env s)
                    `(vector-ref ,v ,pi)))]
             [(vector-expr? v)
              (loop (guarded-list-ref v (add1 pi)) env s)]
             [else `(vector-ref ,(loop v env s) ,pi)]))]
      #;[`(vector-set! ,v ,i ,e)
       (let* ((pi (loop i env s))
              (pe (loop e env s)))
           (cond
             [(symbol? v)
              (let ((val (lookup-defn env v)))
                (if val
                    (begin (vector-set! val pi (cval->val pe)) '(void))
                    `(vector-set! ,v ,pi ,pe)))]
             [(vector-expr? v)
              (loop (guarded-list-set v (add1 pi) pe) env s)]
             [else `(vector-set! ,(loop v env s) ,pi ,pe)]))]

      ;; list operations
      
      [`(for-each ,f ,ls)
       (let ((ex `(if (null? ,ls) (void)
                     (begin (,f (car ,ls))
                            (for-each ,f (cdr ,ls))))))
         (pe-list-op ex ls env s))]
      [`(map ,f ,ls)
       (let ((ex `(if (null? ,ls)
                      '()
                      (cons (,f (car ,ls)) (map ,f (cdr ,ls))))))
         (pe-list-op ex ls env s))]
      [`(foldr ,f ,base ,ls)
       (let ((ex `(if (null? ,ls)
                      ,base
                      (,f (car ,ls) (foldr ,f ,base (cdr ,ls))))))
         (pe-list-op ex ls env s))]
      [`(apply ,f ,ls)
       (let* ((pl (loop ls env s))
              (pf (loop f env s)))
         (if (cval? pl)
             (loop `(,f ,@(cval->val pl)) env s)
             `(apply ,f ,ls)))]

      

      ;; specific cases
      
      ;; let-values
      ;; if e is a values expression, we can assign (possibly multiple) x to e
      ;; after e has been "split" and put into env-form by the function vals
      [`(let-values (((,x ...) ,e)) ,b)
       (let ((pve (loop e env s)))
         (if (or (values-expr? pve) (cval? pve))
             (let* ((ext-env (make-env (map cons x (values-args pve))))
                    (new-env (combine-env ext-env env)))
               (loop b new-env s))
             `(let-values ((,x ,pve)) ,(loop b env s))))]
      ;; leave as is, could be effectful
      [`(let-values () ,b)
       `(let-values () ,(loop b env s))]
      ;; `gen` is a thunk. if the number of values produced by `gen` is known
      ;; then transform into a let-values form, and evaluate again
      [`(call-with-values ,gen ,rec)
       (let* ((vals (partial-eval `(,gen) env))
              (vals-known? (values-arg-count vals)))
         (if vals-known?
             (loop (cwv-to-lv vals rec vals-known?) env s)
             `(call-with-values ,(loop gen env s) ,(loop rec env s))))]
      ;; application
      [`(,rator ,rand ...)
       (let ((prand (map (λ (r) (loop r env s)) rand)))
         (match rator
           [(? symbol?) (let ((ex (lookup-expr env rator)))
                          (if ex
                              (apply-closure (to-clos ex rator env) (map to-sv prand) s)
                              `(,rator ,@prand)))]
           [(? closure?) (apply-closure rator (map to-sv prand) s)]
           [`(lambda ,xs ,b) (loop (apply-closure (to-clos rator 'na env) (map to-sv prand) s) env s)]))]
      [else (error "Not an expression:" e)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;
;;            Closures
;;            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Closure is a (closure Id [Listof Id] Expr Env SpecCache)
;; A SpecCache is [Map [Listof ValT] SClosure]
;; A SClosure is a (spec-clos Id [Listof SymVal] Expr)
(struct spec-clos [name args [body #:mutable]] #:transparent)
(struct closure (name ids body env cache) #:transparent)

;; formals: Expr -> [Listof Id]
;; returns the list of identifiers bound by the expression (if any)
(define (formals expr)
  (match expr
    [`(lambda (,xs ...) ,e) xs]
    [`(lambda ,x ,e) `(,x)]
    [else '()]))

;; to-clos: Expr Id Env -> Closure
;; takes a lambda expression to a closure
(define (to-clos e name env)
  (match e
    [`(lambda (,xs ...) ,body)
     (closure name xs body env (make-hash '()))]))

;; lookup-spec-cache: Closure [Listof SymVal] -> SClosure
;; checks if the arguments have a hit in the specialization cache of a closure
(define (lookup-spec-cache cl args)
  (hash-ref (closure-cache cl)
            (map sv-val args)
            #f))

;; A Stack is a [AssocList Closure [Listof SymVal]]
;; with occasional 'conditional-marker 
(define empty-stack '())

;; mark-stack: Stack -> Stack
(define (mark-stack s)
  (cons 'conditional-marker s))

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
    ;[(eqv? (closure-name (caar stack)) (closure-name cl)) #t]
    [(eqv? (car stack) 'conditional-marker) #t]
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
    [(eqv? (car stack) 'conditional-marker)
     (let-values (((new-args new-stack) (get-args cl args (cdr stack))))
       (values new-args (cons (car stack) new-stack)))]
    [(eqv? (closure-name cl) (closure-name (caar stack))) (values (cdar stack) (cdr stack))]
    [else
     (let-values (((new-args new-stack) (get-args cl args (cdr stack))))
       (values new-args (cons (car stack) new-stack)))]))

;; abstract-types: SymVal SymVal -> SymVal
(define (abstract-args s1 s2)
  (define lub (least-upper-bound (sv-val s1) (sv-val s2)))
  (if (type-tag? lub) (sv lub (gensym)) (sv lub lub)))

;; least-upper-bound: ValT ValT -> ValT
(define (least-upper-bound v1 v2)
  (match `(,v1 ,v2)
    [`(,(? number?) ,(? number?)) 'nat]
    [`(,(? boolean?) ,(? boolean?)) 'bool]
    [`(,(? list?) ,(? list?)) 'pair]
    [`(nat nat) 'nat]
    [else 'value]))

;; specialize-closure: Closure [Listof SymVal] Stack -> Closure
(define (specialize-closure cl args stack)
  (let ((sc (spec-clos (closure-name cl) args "to be replaced")))
    (hash-set! (closure-cache cl) (map sv-val args) sc)
    (set-spec-clos-body! sc (primitive-apply-closure cl args stack))
    sc))

;; primitive-apply-closure: Closure [Listof SymVal] Stack -> Expr
(define (primitive-apply-closure cl args stack)
  (let* ((ids (closure-ids cl))
         (body (closure-body cl))
         (env (closure-env cl))
         (updated-body (update-clos-in-body body cl)))
    (partial-eval-expr updated-body
                       (combine-env (make-env (map cons ids args)) env)
                       (cons (cons cl args) stack))))

;; make-residual-call: SClosure [Listof SymVal] -> Expr
(define (make-residual-call sc args)
  `(,(spec-clos-name sc) ,@(map sv-exp args)))

(define (update-clos-in-body b cl)
  (match b
    [`(,rator ,rands ...)
     (if (eqv? rator (closure-name cl))
         `(,cl ,@rands)
         `(,rator ,@(map (λ (r) (update-clos-in-body r cl)) rands)))]
    [else b]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;
;;            Dealing with multiple values
;;            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; values-expr?: Expr -> Boolean
;; determines if an expression is a values expression
(define (values-expr? e)
  (and (pair? e) (eqv? (car e) 'values)))

;; values-args : Expr -> [List SymVal]
;; extracts the individual values from an expression that can produce multiple values
;; and puts it into the form needed for the environment ([Pair [List Id] Expr])
(define (values-args e)
  (match e
    [`(values ,e ...)
     (map to-sv e)]
    [else (list (to-sv e))]))

;; values-arg-count: Expr -> Number
;; given an expression, determines the number of values produced when evaluated
(define (values-arg-count e)
  (match e
    [`(values ,e ...) (length e)]
    [(? cval?) 1]
    [else #f]))

;; call-with-values-to-let-values (cwv-to-lv): Expr Expr Number -> Expr
;; transforms call-with-values to an equivalent let-values expression
;; given the 'generator' (expression producing multiple values)
;; the number of values this 'generator' produces
;; and the 'receiver', which is called on these values
(define (cwv-to-lv gen rec n)
  (let ((vars (build-list n (lambda (n) (gensym 'x)))))
   `(let-values ((,vars ,gen))
       (,rec ,@vars))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;
;;            Vector helpers
;;            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; vector-expr? : Any -> Boolean
;; vector = '(vector ...)
(define (vector-expr? v)
  (and (pair? v) (eqv? (car v) 'vector)))

(define (guarded-list-ref ls i)
  (if (> i (length ls))
      ls
      (list-ref ls i)))

(define (guarded-list-set ls i v)
  (if (> i (length ls))
      ls
      (list-set ls i v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;
;;            List helpers
;;            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pe-list-op : Expr Expr Env Stack -> Expr
;; given an expression over a list and the list expression,
;; check if the list expression is a symbol and not in the environment
;; if it is not in the environment - just return the expression over the list
;; otherwise, keep partially evaluating
(define (pe-list-op expr ls env s)
  (if (and (symbol? ls)
           (not (in-env? env ls)))
      expr
      (partial-eval-expr expr env s)))
