#lang scheme

#|(define sub1
  (lambda(n)
    (- n 1)))|#
(define *
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else(+ n (* n (sub1 m)))))))
