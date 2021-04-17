(module environments (lib "eopl.ss" "eopl") 
  
  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm. 

  (require "data-structures.scm")

  (provide init-env empty-env extend-env extend-env* apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69

  (define init-env 
    (lambda ()
      (extend-env 
       'i (num-val 1)
       (extend-env
        'v (num-val 5)
        (extend-env
         'x (num-val 10)
         (empty-env))))))

  
  (define (extend-env* vars vals old-env)
    (if (null? vars) ; Again assumaing the vars and the vals are of equal length ... TODO: make this robust plz
        old-env ; If no vars are here, we just return the old environment
        (let ((var (car vars))
              (val (car vals))
              (rest-vars (cdr vars))
              (rest-vals (cdr vals)))
          (let ((extended-env (extend-env var val old-env)))
            (extend-env* rest-vars rest-vals extended-env)))))
;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  ;; Page: 86
  (define apply-env
    (lambda (env search-sym)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "No binding for ~s" search-sym))
        (extend-env (var val saved-env)
	  (if (eqv? search-sym var)
	    val
	    (apply-env saved-env search-sym)))
        (extend-env-rec (p-name b-vars p-body saved-env)
          (if (eqv? search-sym p-name)
            (proc-val (procedure b-vars p-body env))          
            (apply-env saved-env search-sym))))))
    
  )