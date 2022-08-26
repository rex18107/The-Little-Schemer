#lang scheme

; 判断是否为复数
(define (plural? n)
  (cond
    ; 当n等于0时，为结束条件，返回#t
    ((zero? n) #t)
    ; 当n等于1时,为结束条件,返回#f
    ((= n 1) #f)
    ; 对n进行减一后递归
    (else (plural? (sub1 (sub1 n))))))
(plural? 5)
; 判断是否为单数
(define (singular? n)
  (cond
    ; 此数如果不是复数就是单数
    ((plural? n) #f)
    (else #t)))
(singular? 3 )
; 判断是否为质数
;(define ())
