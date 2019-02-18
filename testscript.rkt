#lang rash

(require racket/string racket/system racket/linklet "pe.rkt")

;; version of racket to run (specific to my machine)
(define racket-cmd "../../racket/racket/bin/racket ")

;; version of bootstrap to run (...)
(define bootstrap-cmd "../../racket/racket/src/expander/bootstrap-run.rkt ")

;; extra arguments
(define extra-args "-c compiled/cache-dir/ -sx -t ")

;; name of a racket file
;; string -> string
(define file-name
  (λ (s)
     (string-trim s ".rkt")))

;; generating the command to compile a linklet based on the path of the file
;; string -> string
(define cmd-comp-linklet
  (λ (path)
     (string-append racket-cmd bootstrap-cmd extra-args path " -o " (file-name path) ".linklet")))

;; getting the s-expression version of a file based on the path of the file
;; string -> s-exp
(define file->s-exp
  (λ (path)
     (read (open-input-file path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; beginning of script

cd tests

;; creating linklets

ls *.rkt |>> string-split |> map cmd-comp-linklet |> map system

echo Tests compiled to linklets ...

;; generating the s expressions

(define s-exps #{ls *.linklet |>> string-split |> map file->s-exp}) 

echo S-exps generated ...

(define s-exp-vals (map (λ (l) (instantiate-linklet (compile-linklet l) null))  s-exps))

;echo $s-exp-vals

(define partial-eval-s-exps (map (λ (l) (instantiate-linklet (compile-linklet (partial-eval l)) null))  s-exps))

;echo $partial-eval-s-exps