#lang scheme
(define l '(Jack is a cat))
(define s '(Jack is (a cat)))
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(define lat?
   (lambda (x)
     (cond
     ((null? x) 't)
     ((not (atom? (car x))) 'f)
     (else (lat? (cdr x))))))

