(module lang (lib "eopl.ss" "eopl")                

  ;; grammar for the PROC language
  
  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
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
      (expression ("let" identifier "=" expression "in" expression) let-exp)   
      ; (expression ("proc" "(" identifier ")" expression) proc-exp)
      (expression ("letproc" identifier "(" (separated-list identifier ",") ")" "=" expression "in" expression) letproc-exp)
      ; (expression ("(" expression expression ")") call-exp)

      ;; Multiple possible formal/actual parameters.
      
      (expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp)

      (expression ("dynproc" "(" (separated-list identifier ",") ")" expression) dyn-proc-exp) ;; adding new sy ntax for dynamiclly bound procedure
                                                                                               ;; (As opposed to changing all of the business logic)

      (expression ("traceproc" "(" (separated-list identifier ",") ")" expression) traced-proc-exp)
      (expression ("(" expression (arbno expression) ")") call-exp)
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
