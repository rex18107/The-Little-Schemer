#lang scheme

; 判断是否为偶数
(define (even? n)
  (cond
    ; 当n等于0时，为结束条件，返回#t
    ((zero? n) #t)
    ; 当n等于1时,为结束条件,返回#f
    ((= n 1) #f)
    ; 对n进行减一后递归
    (else (even? (sub1 (sub1 n))))))

; 取反
(define (not n)
  (cond
    ; 此引入参数结果如果为#t，返回#f
    ((eq? n #t) #f)
    (else #t)))
(not (even? 3))

; 判断是否为单数
(define (odd? n)
    ; 调用not函数取反
    (not (even? n)))
(odd? 3 )
(odd? 1001 )
(not (odd? 10))
; 判断是否为质数
;(define ())
