#lang racket

;; NodeInSeq = (Int (listOf<int>) (listOf<int>))

(define (numb->seq n)
  (list n '() '()))

(define (current-element seq)
  (car seq))

(define (insert-to-left n seq)
  (list (current-element seq) (cons n (cadr seq)) (caddr seq)))

(define (insert-to-right n seq)
  (list (current-element seq) (cadr seq) (cons n (caddr seq))))

(define (move-to-left seq)
  (list (caadr seq) (cdadr seq) (cons (current-element seq) (caddr seq))))

(define (move-to-right seq)
  (list (caaddr seq) (cons (current-element seq) (cadr seq)) (cdaddr seq)))
