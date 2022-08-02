#lang scheme
(define insertR
  (lambda(new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) old) (cons old(cons new (cdr lat))))
         (else
          (cons (car lat) (insertR new old (cdr lat)))))))))
          
(define s '(cat oj))
(define l '(banana sugar))