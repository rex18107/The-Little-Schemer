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

; 取反
(define (not-function? n)
  (cond
    ; 此引入参数结果如果为#t，返回#f
    ((eq? n #t) #f)
    (else #t)))
(not-function? (plural? 3))
; 判断是否为单数
(define (singular? n)
  (cond
    ; 如果n是偶数返回#f
    ((plural? n) #f)
    ; 否则即为#t
    (else #t))) 
(singular? 3 )
; 判断是否为质数
;(define ())
