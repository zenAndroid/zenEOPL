(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
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

        (minus-exp (exp1)
                   (let ((val (value-of exp1 env)))
                     ; Huh, at first I didn't have the num-val there
                     ; and that caused quite the issues
                     ; I'll have to remember doing this.
                     (num-val (- (expval->num val)))))

        (addition-exp (term1 term2)
                      (let ((first-val (value-of term1 env))
                            (second-val (value-of term2 env)))
                        (let ((first-number (expval->num first-val))
                              (second-number (expval->num second-val)))
                          (num-val
                           (+ first-number second-number)))))

        (mult-exp (term1 term2)
                  (let ((first-val (value-of term1 env))
                        (second-val (value-of term2 env)))
                    (let ((first-number (expval->num first-val))
                          (second-number (expval->num second-val)))
                      (num-val
                       (* first-number second-number)))))

        (quotient-exp (term1 term2)
                      (let ((first-val (value-of term1 env))
                            (second-val (value-of term2 env)))
                        (let ((first-number (expval->num first-val))
                              (second-number (expval->num second-val)))
                          (num-val
                           (/ first-number second-number)))))


        (num-equality-test (term1 term2)
                           (let ((first-val (value-of term1 env))
                                 (second-val (value-of term2 env)))
                             (let ((first-number (expval->num first-val))
                                   (second-number (expval->num second-val)))
                               (bool-val
                                (equal? first-number second-number)))))
        
        (num-gt-test (term1 term2)
                     (let ((first-val (value-of term1 env))
                           (second-val (value-of term2 env)))
                       (let ((first-number (expval->num first-val))
                             (second-number (expval->num second-val)))
                         (bool-val
                          (> first-number second-number)))))

        (num-lt-test (term1 term2)
                     (let ((first-val (value-of term1 env))
                           (second-val (value-of term2 env)))
                       (let ((first-number (expval->num first-val))
                             (second-number (expval->num second-val)))
                         (bool-val
                          (< first-number second-number)))))

        (cons-exp (head tail)
                  (let ((head-val (value-of head env))
                        (tail-val (value-of tail env)))
                    (pair-val head-val tail-val)))

        (car-exp (list-exp)
                  (let ((list-val (value-of list-exp env)))
                    (expval->pair-car list-val)))

        (cdr-exp (list-exp)
                  (let ((list-val (value-of list-exp env)))
                    (expval->pair-cdr list-val)))

        (null-list-check (expr)
                         (let ((val (value-of expr env)))
                           (bool-val
                            (equal? val (empty-list-val)))))
                                      
        (empty-list-exp ()
                        (empty-list-val))

        ;; List

        (list-op-exp (arg-exps)
                     (list-val (map (lambda (expression)
                                      (value-of expression env))
                                    arg-exps)))


        ;; Cond-exp, noot really certain what and how i am going to catch
        ;; all the elements but it should be doable ...

        ;; Going with overly detailed names, because why not.
        (cond-exp (expressions-to-eval corresponding-consequents)
                  (cond-value expressions-to-eval corresponding-consequents value-of env))

        )))


  )

