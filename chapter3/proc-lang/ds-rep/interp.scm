(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the PROC language, using the data structure
  ;; representation of procedures.

  ;; The \commentboxes are the latex code for inserting the rules into
  ;; the code in the book. These are too complicated to put here, see
  ;; the text, sorry. 

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

  ;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
                   (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (num-val
                       (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
                   (let ((val1 (value-of exp1 env)))
                     (let ((num1 (expval->num val1)))
                       (if (zero? num1)
                           (bool-val #t)
                           (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
                (let ((val1 (value-of exp1 env)))
                  (if (expval->bool val1)
                      (value-of exp2 env)
                      (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
                 (let ((val1 (value-of exp1 env)))
                   (value-of body
                             (extend-env var val1 env))))
        
        (proc-exp (var-list body)
                  (proc-val (procedure var-list body env #f #f))) ; not-traced not-dynamically-scoped

        (traced-proc-exp (var-list body)
                         (proc-val (procedure var-list body env #t #f))) ; yes-traced not-dynamically-scoped
        
        (dyn-proc-exp (var-list body)
                      (proc-val (procedure var-list body (empty-env) #f #t))) ;; Not sure about this, not-traced yes-dynamically-scoped

        (call-exp (rator rand-list)
                  (let ((proc (expval->proc (value-of rator env)))
                        (arg-list (map (lambda(exp) (value-of exp env)) rand-list)))
                    (apply-procedure proc arg-list env)))

        (letproc-exp (proc-name var-list-ident proc-body let-body)

                     ;; letproc foo(arg) = let x = 5 in -(x,arg)
                     ;; We expect the result of this to equal
                     ;; he evaluation of the let body in the context of an environement
                     ;; where the proc-name is bound to the (procedure var-ident proc-body old-env)
                     ;; So we will do just that :thinking:
                     ;; 1 - Extend the old-env by bouonding proc-name to
                     ;; (proc-val (procedure var-ident proc-body env))
                     ;; 2 - evaluate the body of the let in this environement

                     ;; Removed the lets, jst to see how it looks, and i notice its not bad actually
                     ;; can still understand the code fine

                     (value-of let-body
                               (extend-env proc-name
                                           (proc-val (procedure var-list-ident proc-body env #f #f)) ;; By default, let procs arent traced, so ... nor are they dynamically-scoped
                                           env)))
                           

        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc1 value-list env)
      (cases proc proc1
        (procedure (var-list body saved-env traced? dynamically-scoped?)
                   (when traced? (eopl:printf "Entry intro traced proc.\n")) ;; Well, it turns out Racket removed one-armed ifs and replaced them with when/unless.
                   ;; ... shrug
                   (let ((value (value-of body (if (not dynamically-scoped?)
                                                   (extend-env* var-list value-list saved-env)
                                                   (extend-env* var-list value-list env)))))
                     (when traced? (eopl:printf "Exit out of traced proc.\n"))
                     value)))))
  )
