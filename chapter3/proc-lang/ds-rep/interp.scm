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
                  (proc-val (procedure var-list body env)))

        (call-exp (rator rand-list)
                  (let ((proc (expval->proc (value-of rator env)))
                        (arg-list (map (lambda(exp) (value-of exp env)) rand-list)))
                    (apply-procedure proc arg-list)))

        (letproc-exp (proc-name var-list-ident proc-body let-body)
                     ;; letproc foo(arg) = let x = 5 in -(x,arg)
                     ;; We expect the result of this to equal
                     ;; he evaluation of the let body in the context of an environement
                     ;; where the proc-name is bound to the (procedure var-ident proc-body old-env)
                     ;; So we will do just that :thinking:
                     ;; 1 - Extend the old-env by bouonding proc-name to
                     ;; (proc-val (procedure var-ident proc-body env))
                     ;; 2 - evaluate the body of the let in this environement
                     
                       (let ((proc-representation (proc-val (procedure var-list-ident proc-body env))))
                         (let ((new-env (extend-env proc-name proc-representation env)))
                           (value-of let-body new-env))))
                           

        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc1 value-list)
      (cases proc proc1
        (procedure (var-list body saved-env)
                   (value-of body (extend-env* var-list value-list saved-env))))))

  ; Allright, so i need to make it such that the environment that procedure store
  ; only stores the free variable in its body, and leave out all the rest
  ; I will thus obviously need to scan the body too know the free variable
  ; which are identified by being symbols that do not appear among the formal parameters.
  ; This 'scanning' procedure will have to
  ; 1) Identify the symbols in the different expression types
  ; 2) accumulate only the ones that do not appear in the formal parameter list
  ; 3) bundle it all up together
  ; actually, i think a better strat would be to; upon detecting a symbol
  ; - is it on the formal parameter list (is it a member of var list)?
  ; - yes; do nothing
  ; - no; extend an accumulated (initialy empty0 environement
  ; when done => return env


  ;; If this function works as intended, then it will give a list of the free variables.
  ;; We then need to recieve the list from it and extend the empty environment with the free ones.
  ;; God dang i feel like im repeating myself
  (define (free-variable formal-parameters body-expression env)
    ;; Well, since our specification only has one expression per body ...
    (cases expression body-expression
      (const-exp (const)
                 ;; It's a constant ... so not much to do ... thus we return an empty-list
                 '())
      (diff-exp (exp1 exp2)
                ;; -(foo,bar)
                (append (free-variable formal-parameters exp1 env)
                        (free-variable formal-parameters exp2 env)))
      (zero?-exp (exp)
                 (free-variable formal-parameters exp env))
      (if-exp (exp1 exp2 exp3)
              (append (free-variable formal-parameters exp1 env)
                      (free-variable formal-parameters exp2 env)
                      (free-variable formal-parameters exp1 env)))
      (var-exp (ident)
               ;; This is some identifier, so before bundling it up,
               ;; we check it for existence in formal parameters
               ;; if so we DONT add it
               (if (member ident formal-parameters)
                   '() ;; if it is a parameter, then we obviously dont know its value
                   (list ident))) ;; if it is a free variable then add its identifier to the list, so that we may extend the environment with it.
      (let-exp (var exp body)
               (append (list var)
                       (free-variable formal-parameters exp env)
                       (free-variable formal-parameters body env)))
      (proc-exp (var-list body)
                (append var-list
                        (free-variable formal-parameters body env)))
      (call-exp (rator rand-list)
                (let ((rands-vars (append (map (lambda(exp) (free-variable formal-parameters exp env)) rand-list))))
                  (append (free-variable formal-parameters rator env) rands-vars)))
      (letproc-exp (proc-name var-list-ident proc-body let-body)
                   ;; Not using proc-name, since we do not care for it here, the value-of function does that
                   ;; same for let-body
                   ;; ... could be wrong though ... !!!
                   (append var-list-ident
                           (free-variable formal-parameters exp env)))))

  (define (optimize-env env body-exp formal-parameters)
    (let ((free-vars (free-variable formal-parameters body-exp env)))
      (let ((free-vars-val (map (lambda (var) (apply-env env var)) free-vars)))
        (extend-env* free-vars free-vars-val (empty-env)))))
  
  )
