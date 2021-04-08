(module sec2.4 (lib "eopl.ss" "eopl")

  (require "utils.scm")
  
  (define identifier? symbol?)

  (define-datatype lc-exp lc-exp? 
    (var-exp
      (var identifier?))
    (lambda-exp
      (bound-var identifier?)
      (body lc-exp?))
    (app-exp
      (rator lc-exp?)
      (rand lc-exp?)))

  ;; occurs-free? : Sym * Lcexp -> Bool
  (define occurs-free?
    (lambda (search-var exp)
      (cases lc-exp exp
        (var-exp (var) (eqv? var search-var))
        (lambda-exp (bound-var body)
          (and
            (not (eqv? search-var bound-var))
            (occurs-free? search-var body)))
        (app-exp (rator rand)
          (or
            (occurs-free? search-var rator)
            (occurs-free? search-var rand))))))

  ;; test items
  (equal?? (occurs-free? 'x (var-exp 'x)) #t)

  (equal?? (occurs-free? 'x (var-exp 'y)) #f)

  (equal?? (occurs-free? 'x (lambda-exp 'x
                              (app-exp (var-exp 'x) (var-exp 'y))))
    #f)

  (equal??
    (occurs-free? 'x (lambda-exp 'y
                       (app-exp (var-exp 'x) (var-exp 'y))))
    #t)

  (equal??
    (occurs-free? 'x 
      (app-exp
        (lambda-exp 'x (var-exp 'x))
        (app-exp (var-exp 'x) (var-exp 'y))))
    #t)

  (equal?? 
    (occurs-free? 'x
      (lambda-exp 'y
        (lambda-exp 'z
          (app-exp (var-exp 'x)
            (app-exp (var-exp 'y) (var-exp 'z))))))
    #t)

  (define-datatype s-list s-list? 
    (empty-s-list)
    (non-empty-s-list 
      (first s-exp?)
      (rest s-list?)))
  
  (define-datatype s-exp s-exp? 
    (symbol-s-exp
      (sym symbol?))
    (s-list-s-exp
      (slst s-list?)))

  ;; page 48: alternate definition 
  (define-datatype s-list-alt s-list-alt? 
    (an-s-list
      (sexps (list-of s-exp?))))
  
  (define list-of
    (lambda (pred)
      (lambda (val)
        (or (null? val)
          (and (pair? val)
            (pred (car val))
            ((list-of pred) (cdr val)))))))

  ;; For exercises 2.24-2.25
  (define-datatype bintree bintree? 
    (leaf-node 
      (num integer?))
    (interior-node
      (key symbol?) 
      (left bintree?)
      (right bintree?)))

  ;;   > (bintree-to-list
  ;;       (interior-node
  ;;         'a
  ;;         (leaf-node 3)
  ;;         (leaf-node 4)))
  ;;   (interior-node a (leaf-node 3) (leaf-node 4)))

  ;;   > (define tree-1
  ;;       (interior-node 'foo (leaf-node 2) (leaf-node 3)))
  ;;   > (define tree-2
  ;;       (interior-node 'bar (leaf-node -1) tree-1))
  ;;   > (define tree-3
  ;;       (interior-node 'baz tree-2 (leaf-node 1)))
  ;;   > (max-interior tree-2)
  ;;   foo
  ;;   > (max-interior tree-3)
  ;;   baz

  (define-datatype env env?
    (empty-env-repr)
    (extended-env-repr
     (_var identifier?)
     (_val (lambda (v) #t))
     (_env env?)))

  (define (empty-env)
    (empty-env-repr))

  (define (extend-env var val oldEnv)
    (extended-env-repr var val oldEnv))

  (define (apply-env searchVar _env)
    (cases env _env
      (empty-env-repr ()
                      (eopl:error "Environment is empty!"))
      (extended-env-repr (var val oldEnv)
                         (if (eqv? searchVar var)
                             val
                             (apply-env searchVar oldEnv)))))

  (define (has-binding? searchVar _env)
    (cases env _env
      (empty-env-repr ()
                      #f)
      (extended-env-repr (var val oldEnv)
                         (if (eqv? searchVar var)
                             #t
                             (has-binding? searchVar oldEnv)))))

  
  (define e (empty-env))
  (define ee (extend-env 'f 56 e))
  (define ef (extend-env 't 536 ee))
  (define eg (extend-env 'koo 42 ef))

  (equal?? (apply-env 'f eg) 56)
  (equal?? (apply-env 't eg) 536)
  (equal?? (has-binding? 'f eg) #t)
  (equal?? (has-binding? 'd eg) #f)

  (define-datatype stack stack?
    (empty-stack)
    (non-empty-stack
     (stack-top (lambda (v) #t))
     (stack-rest stack?)))


  (define (initial-stack) (empty-stack))

  (define (push elem stackStruct)
    (non-empty-stack elem stackStruct))

  ; To understand why this definition is so curt, try to expand it into
  ; its (cases ...) form, then you will see that under both cases you just tack
  ; the elem onto the thing. (think of cons)


  ; I don't like this very much ... since the actual top value isn't returned
  ; but i suppose we haven't learned how to return multiple values, so ...
  (define (pop stackStruct)
    (cases stack stackStruct
      (empty-stack ()
                   (eopl:error "Cannot pop anything off an empty stack!"))
      (non-empty-stack (top rest)
                       rest)))

  (define (peek stackStruct)
    (cases stack stackStruct
      (empty-stack ()
                   (eopl:error "Cannot pop anything off an empty stack!"))
      (non-empty-stack (top rest)
                       top)))

  (define (empty-stack? stackStruct)
    (cases stack stackStruct
      (empty-stack () #t)
      (non-empty-stack (top rest)
                       #f)))

  (define oneElem (push 4 (initial-stack)))
  (define twoElem (push 5 oneElem))
  (define threeElem (push "Yahoo" twoElem))
  
  

  (eopl:printf "unit tests completed successfully.~%")

  )
