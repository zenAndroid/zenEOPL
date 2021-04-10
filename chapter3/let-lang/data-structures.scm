(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for let-lang.

  (provide (all-defined-out))               ; too many things to list

  ;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

  ;;; an expressed value is either a number, a boolean or a procval.
  ;;; In addition, i will now add a pair as a basic expval.
  
  (define-datatype expval expval?
    (num-val
     (value number?))
    (bool-val
     (boolean boolean?))
    (empty-list-val)
    (pair-val
     (head expval?)
     (tail expval?)))

  ;;; extractors:

  ;; expval->num : ExpVal -> Int
  ;; Page: 70
  (define expval->num
    (lambda (v)
      (cases expval v
        (num-val (num) num)
        (else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  ;; Page: 70
  (define expval->bool
    (lambda (v)
      (cases expval v
        (bool-val (bool) bool)
        (else (expval-extractor-error 'bool v)))))

  (define expval->pair-car
    (lambda (v)
      (cases expval v
        (pair-val (head tail) head)
        (else (expval-extractor-error 'pair-car v)))))

  (define expval->pair-cdr
    (lambda (v)
      (cases expval v
        (pair-val (head tail) tail)
        (else (expval-extractor-error 'pair-cdr v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                  variant value)))

  (define (list-val args)
    (if (null? args)
        (empty-list-val)
        (pair-val (car args)
                  (list-val (cdr args)))))

  ;; I have no clue what i'm doing atm

  (define (cond-value conds conseqs evaluator env) ; rip this solution is too hacky i dont think its the one intended
    ; I will make the assumption that the two lists
    ; have the same length, TODO: Is this something you want to keep?
    (if (null? conds)
        (eopl:error "Cond expression empty")
        (let ((head-cond-exp (car conds))
              (head-consequent-exp (car conseqs)))
          (let ((head-val (evaluator head-cond-exp env)))
            (if (expval->bool head-val)
                (evaluator head-consequent-exp env)
                (cond-value (cdr conds) (cdr conseqs) evaluator env))))))

  ;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

  ;; example of a data type built without define-datatype

  (define empty-env-record
    (lambda () 
      '()))

  (define extended-env-record
    (lambda (sym val old-env)
      (cons (list sym val) old-env)))
  
  (define empty-env-record? null?)
  
  (define environment?
    (lambda (x)
      (or (empty-env-record? x)
          (and (pair? x)
               (symbol? (car (car x)))
               (expval? (cadr (car x)))
               (environment? (cdr x))))))

  (define extended-env-record->sym
    (lambda (r)
      (car (car r))))

  (define extended-env-record->val
    (lambda (r)
      (cadr (car r))))

  (define extended-env-record->old-env
    (lambda (r)
      (cdr r)))

  )
