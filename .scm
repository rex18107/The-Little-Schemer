#lang scheme

; p81
(define (rember* a l)
  (cond
    ((null? l) '())
    ((list? (car l))
     (cond
       ((null? (car (car l))) '())
       ((eq? a (car (car l))) (rember* a (car (cdr l))))
       (else (cons (car (car l)) (rember* a (car l)))))
     (else ((eq? a (car l)) (cons (car l) (rember* a (cdr l))))))))
        
