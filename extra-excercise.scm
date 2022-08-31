#lang scheme

; 取反
(define (not n)
  (cond
    ; 此引入参数结果如果为#t,返回#f
    ((eq? n #t) #f)
    (else #t)))

; 判断是否为偶数
(define (even? n)
  (cond
    ; 当n等于0时，为结束条件，返回#t
    ((zero? n) #t)
    ; 当n等于1时,为结束条件,返回#f
    ((= n 1) #f)
    ; 对n进行减一再减一后调用函数递归相当于是减二
    (else (even? (sub1 (sub1 n))))))
(even? 3)

; 判断偶数的第二种写法，引入参数flag，初始值为#f或#t都可
; 只要进行了单数次递归就是复数#t,进行偶数次递归就是奇数,返回#f
(define (even n flag)
  (cond ((= n 0) flag)
        ; 对n减1，且对flag的值取反后进行递归
        (else (even (- n 1) (not flag))))
  )

(define (even-v2? n)
  (even n #t) 
  )
(even-v2? 7)

; 判断是否为单数
(define (odd? n)
    ; 调用not函数取反
    (not (even? n)))
(odd? 3 )
(odd? 1001 )

(not (odd? 10))
(not (even? 3))
; 判断是否为质数
;(define ())
