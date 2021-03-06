(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LETREC language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

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
  ;; Page: 83
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
                  (proc-val (procedure var-list body env)))

        (call-exp (rator rand-list)
                  (let ((proc (expval->proc (value-of rator env)))
                        (arg-list (map (lambda(exp) (value-of exp env)) rand-list)))
                    (apply-procedure proc arg-list))) ;; Not passing the environement since there are no dynamic binding shenanigans here.

        (letrec-exp (p-name b-vars p-body letrec-body)
                    (value-of letrec-body
                              (extend-env-rec p-name b-vars p-body env)))

        (mult-proc-letrec-exp (procs-names one-var-per-proc proc-bodies letrec-body)
                              ;; The (value-of the letrec-body)
                              ;; in the environment containing all the procs
                              ;; ... :thinking
                              (let ((extended-env (extend-env-rec* procs-names one-var-per-proc proc-bodies env)))
                                (begin
                                  (pretty-print extended-env) (newline))
                                (value-of letrec-body
                                          extended-env)))

        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal

  (define apply-procedure
    (lambda (proc1 arg-list)
      (cases proc proc1
        (procedure (var-list body saved-env)
                   (value-of body (extend-env* var-list arg-list saved-env))))))

  (trace apply-procedure)
  
  )
  


  
