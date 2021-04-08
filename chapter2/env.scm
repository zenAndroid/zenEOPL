(module sec2.2-ds-rep (lib "eopl.ss" "eopl")

  ;; Simple data structure representation of environments
  ;; Page: 38

  (require "utils.scm")

  ;; data definition:
  ;; Env ::= (empty-env) | (extend-env Var Schemeval Env)

  ;; empty-env : () -> Env
  (define empty-env
    (lambda () (list 'empty-env)))
  
  ;; extend-env : Var * Schemeval * Env -> Env
  (define extend-env
    (lambda (var val env)
      (list 'extend-env var val env)))
  
  ;; apply-env : Env * Var -> Schemeval
  (define apply-env
    (lambda (env search-var)
      (cond
        ((eqv? (car env) 'empty-env)
         (report-no-binding-found search-var))
        ((eqv? (car env) 'extend-env)
         (let ((saved-var (cadr env))
               (saved-val (caddr env))
               (saved-env (cadddr env)))
           (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
        (else
          (report-invalid-env env)))))
  
  (define report-no-binding-found
    (lambda (search-var)
      (eopl:error 'apply-env "No binding for ~s" search-var)))
  
  (define report-invalid-env
    (lambda (env)
      (eopl:error 'apply-env "Bad environment: ~s" env)))

  (define e
    (extend-env 'd 6
      (extend-env 'y 8
        (extend-env 'x 7
          (extend-env 'y 14
            (empty-env))))))

  (equal?? (apply-env e 'd) 6)
  (equal?? (apply-env e 'y) 8)
  (equal?? (apply-env e 'x) 7)

  (report-unit-tests-completed 'apply-env)

  (define my-empty-env '())

  (define (my-env-extend var val old-env)
    (cons (cons var val) old-env))

  (define (my-apply-env env var)
    (cond ((null? env) (report-no-binding-found var))
          ((eqv? var (caar env)) (cdar env))
          (else (my-apply-env (cdr env) var))))

  (define feeee (my-env-extend 'foo 1 (my-env-extend 'gee 98 (my-env-extend 'll 343 (my-env-extend 'j 23 my-empty-env)))))
)
