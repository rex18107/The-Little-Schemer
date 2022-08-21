#lang scheme

; 判断是否为复数
(define (plural? n)
  (cond
    ((zero? n) #t)
    ((= n 1) #f)
    (else (plural? (sub1 (sub1 n))))))
(plural? 5)
; 判断是否为复数
(define (singular? n)
  (cond
    ((plural? n) #f)
    (else #t)))
(singular? 3 )
; 判断是否为质数
(define ())
