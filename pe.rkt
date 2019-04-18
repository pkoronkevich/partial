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
;; - (PrimOp Expr ...) ;; general builtins, like +, list, etc
;; - (EffectOp Expr ...) ;; effectful operators, like make-struct-type 
;; - (if Expr Expr Expr)
;; - (lambda (Id ...) Expr)
;; - (lambda Id Expr)
;; - (begin Expr ...)
;; - (Expr Expr ...) ;; application

;; val->cval: Value -> CodeValue
;; converts Values to CodeValues (s expressions)
(define (val->cval v) 
  (match v
    [(or (? number?) (? boolean?) (? string?)) v]
    [(? vector?) `(vector ,@(vector->list v))]
    [else `(quote ,v)]))

(module+ test (require rackunit) 
  (check-equal? (val->cval 3) 3)
  (check-equal? (val->cval #t) #t)
  (check-equal? (val->cval "hi sam") "hi sam")
  (check-equal? (val->cval '()) ''())
  (check-equal? (val->cval '(1 2 3)) ''(1 2 3))
  (check-equal? (val->cval 'a) ''a)
  (check-equal? (val->cval 'c) ''c)
  (check-equal? (val->cval (vector 1 2 3)) '(vector 1 2 3)))


;; cval?: Expr -> Boolean
(define (cval? e)
  (match e
    [(or (? number?) (? boolean?) (? string?)) #t] ;; self-quoting
    [`(quote ,e) #t]
    [`(vector ,e ...) (andmap cval? e)]
    [else #f]))

(module+ test
  (check-equal? (cval? '3) #t)
  (check-equal? (cval? '#t) #t)
  (check-equal? (cval? '"hi sam") #t)
  (check-equal? (cval? ''()) #t)
  (check-equal? (cval? ''(1 2 3)) #t)
  (check-equal? (cval? ''(hi hello)) #t)
  (check-equal? (cval? ''a) #t)
  (check-equal? (cval? ''c) #t)
  (check-equal? (cval? '(vector 1 2 3)) #t)
  (check-equal? (cval? '(add1 1)) #f)
  (check-equal? (cval? '(list 1 3 4)) #f)
  (check-equal? (cval? '(+ 4 5 6)) #f))

;; cval->val: CodeValue -> [Maybe Value]
;; converts CodeValues (s expressions) to Values (Racket's values)
(define (cval->val v)
  (match v
    [(or (? number?) (? boolean?) (? string?)) v] ;; self-quoting
    [`(quote ,e) e]
    [`(vector ,e ...) (apply vector e)]
    [else v]))

(module+ test
  (check-equal? (cval->val '3) 3)
  (check-equal? (cval->val '#t) #t)
  (check-equal? (cval->val '"hi sam") "hi sam")
  (check-equal? (cval->val ''()) '())
  (check-equal? (cval->val ''(1 2 3)) '(1 2 3))
  (check-equal? (cval->val ''(hi hello)) '(hi hello))
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
                  cons car cdr list list-ref
                  vector vector-length vector-ref
                  string-append null? vector? 
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
;; - 'string
;; - 'bool
;; - 'func
;; - 'pair
;; - 'value

(define (type-tag? t)
  (memv t '(nat string sym bool func pair value)))

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

;; to-exp: [U SymVal Closure Expr] -> Expr
;; extracts the exp field of the given SymVal, if it is one
;; otherwise recreates the closure, if it is one
(define (to-exp e)
  (match e
    ((? sv?) (sv-exp e))
    ((? closure?) (clos->exp e))
    (`(,es ...) (map to-exp es))
    (else e)))

;; split prim-ops into groups based on the type returned

(define nat-ops
  (make-ops + - * / sub1 add1))

;; nat-op? : Any -> Boolean
(define (nat-op? p)
  (hash-ref nat-ops p #f))

(define bool-ops
  (make-ops null? exact-integer? zero? procedure?
            < > >= <=
            procedure-arity-includes? pair? = equal?))

;; bool-op? : Any -> Boolean
(define (bool-op? p)
  (hash-ref bool-ops p #f))

(define pair-ops
  (make-ops cons list map))

;; pair-op? : Any -> Boolean
;; TODO more ops?
(define (pair-op? p)
  (hash-ref pair-ops p #f))

(define str-ops
  (make-ops string-append))

;; str-op? : Any -> Boolean
(define (str-op? p)
  (hash-ref str-ops p #f))


;; get-tt: Expr -> TypeTag
(define (get-tt e)
  (match e
    [`(,(? nat-op?) ,es ...) 'nat]
    [`(,(? bool-op?) ,es ...) 'bool]
    [`(,(? pair-op?) ,es ...) 'pair]
    [`(,(? str-op?) ,es ...) 'string]
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
(define (partial-eval-expr exp env stack)
  (let ((e (partial-eval-loop exp env stack)))
    (match e
      [(? sv?) (sv-exp e)]
      [(? closure?) (clos->exp e)]
      [else e])))


;; partial-eval-loop : [U Expr Symval] Env Stack -> [U Expr SymVal Closure]
;; the stack is used for termination (see [Closures] section)
;; note that occasionally call partial-eval-expr instead of loop
;; this occurs when recurring is not in tail position
;; so possible symbolic values will not be interspersed within expressions
(define (partial-eval-loop expr env stack)
  (let loop ([e expr]
             [env env]
             [s stack])
    (match e
      ;; the basic primitives
      [(? cval?) (to-sv e)]
      ['(void) e]
      ;; symbol case
      [(? symbol?)
       (let ((expr (lookup-expr env e)))
         (cond                         ;; cases to avoid error/infinite loops
           [(not expr) e]              ;; (1) identifier is not in the environment
           [(eqv? expr e) e]           ;; (2) identifier mapped to itself
           [(in-expr? e expr) expr]    ;; (3) identifier mapped to some version of itself (i.e. x -> (sub1 x))
           [else (loop expr env s)]))] ;; (4) otherwise loop
      ;; can replace with result if all arguments are known
      ;; TODO: what if arguments are effectful, i.e. read?
      [`(,(? prim-op? p) ,e ...)
       (let ((es (map (lambda (x) (loop x env s)) e)))
         (if (andmap sv? es)
             (let* ((op (get-prim-op p))
                    (vs (map sv-val es))
                    (res (apply op vs)))
              (to-sv (val->cval res))) 
             (let ((exs (map to-exp es)))
                `(,p ,@exs))))]
      ;; for now ignoring "effectful" (struct) ops
      [`(,(? effect-op? c) ,e ...)
       (let ((es  (map (lambda (x) (partial-eval-expr x env s)) e))) 
         `(,c ,@es))]
      ;; if
      [`(if ,p ,t ,f)
       (let ((pv (loop p env s)))
         (if (sv? pv)
             (if (sv-val pv) (loop t env s) (loop f env s))
             `(if ,pv
                  ,(partial-eval-expr t env (mark-stack s))
                  ,(partial-eval-expr f env (mark-stack s)))))]
      ;; (begin e) == e
      [`(begin ,e) (loop e env s)]
      [`(begin ,es ...)
       `(begin ,@(map (lambda (e) (partial-eval-expr e env s)) es))]
      ;; lambda case
      [`(lambda ,x ,b)
       (exp->clos `(lambda ,x ,(partial-eval-expr b env s)) 'na env)]
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
         (pe-list-op e ex ls env s))]
      [`(map ,f ,ls)
       (let ((ex `(if (null? ,ls)
                      '()
                      (cons (,f (car ,ls)) (map ,f (cdr ,ls))))))
         (pe-list-op e ex ls env s))]
      [`(foldr ,f ,base ,ls)
       (let ((ex `(if (null? ,ls)
                      ,base
                      (,f (car ,ls) (foldr ,f ,base (cdr ,ls))))))
         (pe-list-op e ex ls env s))]
      [`(apply ,f ,ls)
       (let* ((pl (loop ls env s))
              (pf (partial-eval-expr f env s)))
         (if (sv? pl)
             (loop `(,f ,@(sv-val pl)) env s)
             `(apply ,f ,ls)))]
      ;; specific cases
      
      ;; let-values
      ;; if e is a values expression, we can assign (possibly multiple) x to e
      ;; after e has been "split" and put into env-form by the function values-args
      [`(let-values (((,x ...) ,e)) ,b)
       (let ((pve (loop e env s)))
         (if (or (values-expr? pve) (sv? pve))
             (let* ((ext-env (make-env (map cons x (values-args pve))))
                    (new-env (combine-env ext-env env)))
               (loop b new-env s))
             `(let-values ((,x ,pve)) ,(partial-eval-expr b env s))))]
      ;; leave as is, could be effectful
      [`(let-values () ,b)
       `(let-values () ,(partial-eval-expr b env s))]
      ;; `gen` is a thunk. if the number of values produced by `gen` is known
      ;; then transform into a let-values form, and evaluate again
      [`(call-with-values ,gen ,rec)
       (let* ((vals (loop `(,gen) env s))
              (vals-known? (values-arg-count vals)))
         (if vals-known?
             (loop (cwv-to-lv vals rec vals-known?) env s)
             `(call-with-values
               ,(partial-eval-expr gen env s)
               ,(partial-eval-expr rec env s))))]
      ;; application
      [`(,rator ,rand ...)
       (let ((prand (map (λ (r) (loop r env s)) rand))
             (to-sv-or-not-to-sv (lambda (ex) (if (sv? ex) ex (to-sv ex)))))
         (match rator
           ;; identifier case
           [(? symbol?) (let ((ex (lookup-expr env rator)))
                          (if ex
                              (apply-closure (exp->clos ex rator env) (map to-sv-or-not-to-sv prand) s)
                              `(,rator ,@(map to-exp prand))))]
           ;; lambda case TODO safe to loop? anonymous functions shouldn't recur...
           [`(lambda ,xs ,b)
            (let ((v (apply-closure (exp->clos rator 'na env) (map to-sv-or-not-to-sv prand) s)))
              (loop v env s))]
           [else `(,(partial-eval-expr rator env s) ,@(map to-exp prand))]))]
      [(? sv?) e]
      [else (error "Not an expression:" e)])))

;; in-expr?: Id Expr -> Bool
;; determines if an identifier is in an expression
(define (in-expr? id expr)
  (match expr
    [(? symbol?) (eqv? id expr)]
    [`(,es ...)
     (ormap (λ (ex) (in-expr? id ex)) es)]
    [else #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;
;;            Closures
;;            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Closure is a (closure Id [U Id [Listof Id]] Expr Env SpecCache)
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

;; clos->exp: Expr Id Env -> Closure
;; takes a lambda expression to a closure
(define (exp->clos e name env)
  (match e
    [`(lambda (,xs ...) ,body)
     (closure name xs body env (make-hash '()))]
    [`(lambda ,x ,body)
     (closure name x body env (make-hash '()))]
    [else (error "Not a closure:" e)]))

;; clos->exp: Closure -> Expr
(define (clos->exp e)
  `(lambda ,(closure-ids e) ,(closure-body e)))

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
;; places a conditional marker on the stack in order to indicate a speculative call
(define (mark-stack s)
  (cons 'conditional-marker s))

;; apply-closure: Closure [Listof SymVal] Stack -> Expr

;; there are several cases that this function covers:
;; (1) the simplest case (else case)
;;     - there is no hit in the specialization cache of the closure,
;;     - there is no "active" or "speculative" call on the stack
;;     - then we simply unfold the call with `primitive-apply-closure`
;; (2) there is a hit in the specialization cache (first case)
;;     - make a residual call to the specialized closure
;;     - which is simply generating code applying the closure name onto the arguments
;; (3) "speculative" call is found on the stack
;;     - specialize? case
;;     - here, we see if there is a conditional marker on the stack
;;     - and a call to the current closure (could be onto different args!)
;;     - we find the first one in this case, paper mentions "most similar" (but not sure what that means really)
;;     - and return it along with the arguments (cl2+args)
;;     - we generalize using these arguments, push the current closure and the new arguments onto the stack
;;     - check for a hit in the specialization cache !! OF THE CLOSURE FROM THE STACK !!
;;     - then generate a residual call using that if so
;;     - otherwise generate a residual call to a specialized closure (see specialize-closure)
(define (apply-closure cl args stack)
  (cond
    [(lookup-spec-cache cl args) =>
     (λ (sc) (make-residual-call sc args))]
    [(specialize? cl args stack) =>
     (λ (cl2+args)
       (let* ((new-args (generalize-arguments args (cdr cl2+args)))
              (new-stack (cons (cons cl new-args) stack))
              (sc (lookup-spec-cache (car cl2+args) new-args)))
         (if sc
             (make-residual-call sc new-args)
             (make-residual-call (specialize-closure cl new-args new-stack) args))))]
    [else (primitive-apply-closure cl args stack)]))

;; specialize? : Closure [Listof SymVal] Stack -> Bool
;; checks if there is a conditional marker on the stack, if so,
;; finds any active closure (returns the current active closure if none found).
(define (specialize? cl args stack)
  (cond
    [(null? stack) #f]
    [(eqv? (car stack) 'conditional-marker) (get-clos+args cl args stack)]
    [else (specialize? cl args (cdr stack))]))

;; get-clos+args: Closure [Listof SymVal] Stack -> [Pairof Closure [Listof SymVal]]
;; finds the first instance of a closure (and the args it was applied to) on the stack with the same name
;; otherwise returns the given closure and arguments
(define (get-clos+args cl args stack)
  (cond
    [(null? stack) `(,cl . ,args)]
    [(eqv? (car stack) 'conditional-marker)
     (get-clos+args cl args (cdr stack))]
    [(eqv? (closure-name cl) (closure-name (caar stack))) (car stack)]
    [else (get-clos+args cl args (cdr stack))]))

;; generalize-arguments: [Listof SymVal] [Listof SymVal] -> [Listof SymVal]
;; given two lists of arguments, finds the least upper bound between them
(define (generalize-arguments args stack-args)
  (map abstract-args stack-args args))

;; abstract-args: SymVal SymVal -> SymVal
;; given two sv's, finds the least upper bound
;; generate a random id if results in a type tag
;; TODO (which it seems like it always will) TODO
(define (abstract-args s1 s2)
  (define lub (least-upper-bound (sv-val s1) (sv-val s2)))
  (if (type-tag? lub) (sv lub (gensym)) (sv lub lub)))

;; least-upper-bound: ValT ValT -> ValT
;; least upper bound between values/type tags
(define (least-upper-bound v1 v2)
  (match `(,v1 ,v2)
    [`(,(? number?) ,(? number?)) 'nat]
    [`(,(? boolean?) ,(? boolean?)) 'bool]
    [`(,(? list?) ,(? list?)) 'pair]
    [`(nat nat) 'nat]
    [`(bool bool) 'bool]
    [`(pair pair) 'pair]
    [else 'value]))

;; specialize-closure: Closure [Listof SymVal] Stack -> Closure
;; here we create a new specialized closure (with the same name, is this correct? TODO)
;; update the current closure's cache with this new specialized closure
;; then set the spec closure's body applying the closure onto the arguments
(define (specialize-closure cl args stack)
  (let ((sc (spec-clos (closure-name cl) args "to be replaced")))
    (hash-set! (closure-cache cl) (map sv-val args) sc)
    (set-spec-clos-body! sc (primitive-apply-closure cl args stack))
    sc))

;; primitive-apply-closure: Closure [Listof SymVal] Stack -> Expr
;; applying a closure as one normally would (unfolding)
(define (primitive-apply-closure cl args stack)
  (let* ((ids (closure-ids cl))
         (body (closure-body cl))
         (env (closure-env cl)))
    (cond
      [(symbol? ids) (let* ((vals (map sv-val args))
                            (exprs (map sv-exp args))
                            (all-vals? (andmap (lambda (v) (not (type-tag? v))) vals)))
                       (if all-vals?
                           (let ((s (sv `(,@vals) `(list ,@exprs))))
                             (partial-eval-expr body
                               (extend env ids s)
                               (cons (cons cl s) stack)))
                           (let ((s (sv 'pair `(list ,@exprs))))
                             (partial-eval-loop body
                               (extend env ids s)
                               (cons (cons cl s) stack)))))]
      [else (partial-eval-loop body
                               (combine-env (make-env (map cons ids args)) env)
                               (cons (cons cl args) stack))])))

;; make-residual-call: SClosure [Listof SymVal] -> Expr
;; makes the code for a residual function call
(define (make-residual-call sc args)
  `(,(spec-clos-name sc) ,@(map sv-exp args)))


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
;; and puts it into the form needed for the environment (SymVal)
(define (values-args e)
  (match e
    [`(values ,e ...)
     (map to-sv e)]
    [(? sv?) (list e)]
    [else (list (to-sv e))]))

;; values-arg-count: Expr -> Number
;; given an expression, determines the number of values produced when evaluated
(define (values-arg-count e)
  (match e
    [`(values ,e ...) (length e)]
    [(? sv?) 1]
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
;;            List helpers
;;            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pe-list-op : Expr Expr Expr Env Stack -> Expr
;; given an expression over a list and the list expression,
;; check if the list expression is a symbol and not in the environment
;; if it is not in the environment - just return the original expression over the list
;; otherwise, keep partially evaluating
(define (pe-list-op orig-expr expr ls env s)
  (if (and (symbol? ls)
           (not (in-env? env ls)))
      orig-expr
      (partial-eval-loop expr env s)))
