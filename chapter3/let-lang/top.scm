(module top (lib "eopl.ss" "eopl")
  
  ;; top level module.  Loads all required pieces.
  ;; Run the test suite with (run-all).

  (require "drscheme-init.scm")
  (require "data-structures.scm")  ; for expval constructors
  (require "lang.scm")             ; for scan&parse
  (require "interp.scm")           ; for value-of-program
  (require "tests.scm")            ; for test-list
  
  ;; since this is the top-level module, we don't really need to
  ;; provide anything, but we do so just in case.  

  (provide run run-all)

  (provide test-all)

  (define (test-all) (run-all))

  ;; here are some other things that could be provided:

  ;;   (provide (all-defined-out))
  ;;   (provide (all-from "interp.scm"))
  ;;   (provide (all-from "lang.scm"))
  
  ;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;
  
  ;; run : String -> ExpVal
  ;; Page: 71
  (define run
    (lambda (string)
      (value-of-program (scan&parse string))))
  
  ;; run-all : () -> unspecified
  
  ;; runs all the tests in test-list, comparing the results with
  ;; equal-answer?  

  (define run-all
    (lambda ()
      (run-tests! run equal-answer? test-list)))
  
  (define equal-answer?
    (lambda (ans correct-ans)
      (equal? ans (sloppy->expval correct-ans))))
  
  (define sloppy->expval 
    (lambda (sloppy-val)
      (cond
        ((number? sloppy-val) (num-val sloppy-val))
        ((boolean? sloppy-val) (bool-val sloppy-val))
        (else
         (eopl:error 'sloppy->expval 
                     "Can't convert sloppy value to expval: ~s"
                     sloppy-val)))))
    
  ;; run-one : symbol -> expval

  ;; (run-one sym) runs the test whose name is sym
  
  (define run-one
    (lambda (test-name)
      (let ((the-test (assoc test-name test-list)))
        (cond
          ((assoc test-name test-list)
           => (lambda (test)
                (run (cadr test))))
          (else (eopl:error 'run-one "no such test: ~s" test-name))))))


  (run-all)

  (newline)(display "=== Environment operations: ===") (newline)(newline)
  
  (display (run "x")) (newline)
  (display (run "v")) (newline)
  (display (run "i"))(newline)
  (display (run "10"))(newline)
  (display (run "-(1, 2)"))(newline)
  (display (run "-(1, x)"))(newline)

  ;; (run "foo") -> error
  (newline)(display "=== If evaluation order test operations: ===") (newline)(newline)

  (display (run "if zero?(-(11, 11)) then 3 else 4"))(newline)

  (display (run "minus(4)"))(newline)

  (display (run "if zero?(-(11, 11)) then minus(3) else minus(4)")) (newline)

  (newline)(display "=== List operations: ===") (newline)(newline)

  (display (run "car(cons(cons(5,6),6))")) (newline)

  (display (run "cons(5,cons(6,cons(7,emptylist)))")) (newline)

  (display (run "null?(emptylist)")) (newline)

  (display (run "list(1,2,let x = -(10,7) in plus(x,3))")) (newline)

  (display (run "car(list(1,2,let x = -(10,7) in plus(x,3)))")) (newline)

  (display (run "cond zero?(5) ==> 5 greater?(-(11,11),minus(9)) ==> 4 end")) (newline)
  
  ;; That ... worked????
  ;; first time ????
  ;; ????
  ;; I am going to sleep.
  
  (run "plus(plus(1,1),4)")
  ; This doesn't print anything, i thought it was a bug
  ; But i think it is intended since the other solutions do this AND
  ; the evaluator dosent yet know how to handle raw values i assume
  ; (like, if you give it a raw number, itll be fine, but if you feed it a #(struct:num-val 8) value
  ; it doesnt have a case for how to handle with that, weird, hope that gets solved i guess
  
  (newline)(display "Printing operations:") (newline)(newline)

  (run "print(3)")
 
  (run "print(cons(1,2))")

  (run "print(list(1,2))")

  (run "plus(print(66),print(8))")

  (run "let x = 1 in list(x)") ; This also doesn't print anything, but it passes the tests :/
  ; Eh, annoying but oh well ...
  
  
  )




