(module lang

  ;; grammar for the LET language

  (lib "eopl.ss" "eopl")                
  
  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)
      (expression (number) const-exp)
      (expression ("-" "(" expression "," expression ")") diff-exp)
      (expression ("zero?" "(" expression ")") zero?-exp)
      (expression ("if" expression "then" expression "else" expression) if-exp)
      (expression (identifier) var-exp)
      ; (expression ("let" identifier "=" expression "in" expression) let-exp)
      ; New let
      (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
      (expression ("let*" (arbno identifier "=" expression) "in" expression) let-star-exp)
      
      ;; My additions

      ;; Arithmetic
      (expression ("minus" "(" expression ")") minus-exp)
      (expression ("plus" "(" expression "," expression ")") addition-exp)
      (expression ("mult" "(" expression "," expression ")") mult-exp)
      (expression ("div" "(" expression "," expression ")") quotient-exp)

      ;; Predicates ...
      (expression ("equal?" "(" expression "," expression ")") num-equality-test)
      (expression ("greater?" "(" expression "," expression ")") num-gt-test)
      (expression ("less?" "(" expression "," expression ")") num-lt-test)

      ;; List processing operations ...

      (expression ("cons" "(" expression "," expression ")") cons-exp)
      (expression ("car" "(" expression ")") car-exp)
      (expression ("cdr" "(" expression ")") cdr-exp)
      (expression ("emptylist") empty-list-exp)
      (expression ("null?" "(" expression ")") null-list-check)

      ;; Actual list operator

      ;; Got the (separated-list ...) from their description of SLLGEN,
      ;; They never said you had to look for it, this is a thing i greatly dislike >:(
      ;; Hope it works though.

      (expression ("list" "(" (separated-list expression ",") ")") list-op-exp)

      ;; Cond-exps

      (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)

      ;; Print operation

      (expression ("print" "(" expression ")") print-exp)
      
      ))
  
  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
