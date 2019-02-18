#lang racket



;; partial-eval-app : Expr Env -> Expr
#;(define (partial-eval-app expr env)
  (match expr
    [`((lambda (,x ...) ,b) ,args ...)
     ;; eval the arguments
     (let ((argvs (map (lambda (a) (partial-eval-expr a env)) args)))
       ;; evaluate the body after substitution
       (partial-eval-expr (subst (map cons x argvs) b) env))]
    [`(,f ,args ...)
     ;; look up the definition, eval the arguments
     (let ((fdef (lookup env f))
           (argvs (map (lambda (a) (partial-eval-expr a env)) args)))
       (if fdef
           (let* ((body (defn fdef)) ;; get the body of the defn
                  (vars (ids fdef))  ;; get the arguments
                  (mapping (if (list? vars)                          ;; could be single or multi arguments
                               (map cons vars argvs)                 ;; if not in a list, assign the variable to a list of the args
                               (list (cons vars (val->cval argvs))))))
             (if (ormap cval? argvs) ;; v dum way to check if it will terminate (actually doesn't lmao)
                 (partial-eval-expr (subst mapping body) env) ;; evaluate after substitution
                 (subst mapping body)))           
           `(,f ,@argvs)))]
    [else (error "Not an application:" expr)]))

;; subst : [AssocList Id Expr] Expr -> Expr
;; pretty straightforward substitution
#;(define (subst mapping e)
  (let loop ([m mapping] [e e])
    (match e
      [(? symbol?) (let ((assoc (assv e m)))
                     (if assoc (cdr assoc) e))]
      [(? cval?) e]
      [`(,(? prim-op? p) ,es ...) `(,p ,@(map (lambda (e) (loop m e)) es))]
      [`(if ,p ,t ,f) `(if ,(loop m p) ,(loop m t) ,(loop m f))]
      [`(lambda (,x2) ,b) (if (memv x2 (map car m))
                              e
                              `(lambda (,x2) ,(loop m b)))]
      [`(lambda ,x2 ,b) (if (memv x2 (map car m))
                            e
                            `(lambda ,x2 ,(loop m b)))]
      [`(,es ...) (map (lambda (e) (loop m e)) es)])))