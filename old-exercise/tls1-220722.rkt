#lang scheme
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(define lat?
   (lambda (x)
     (cond
     ((null? x) 't)
     ((not (atom? (car x))) 'f)
     (else (lat? (cdr x))))))
(define member?
  (lambda (a lat)
    (cond
      ((null? lat)#f)
      (else(or (eq? (car lat) a)
               (member? a (cdr lat)))))))
(define a 'apple)
(define l '(apple is red))
(member? a l)