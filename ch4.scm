#lang scheme

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; p59
(atom? 14); #t,全部的数字都是原子
(atom? -3); #t，但这里不考虑负数
(atom? 3.2321); #t,但这里只考虑整数

(define (add1 n); add1函数是指在参数的值上加一
  (+ n 1)); 给出add1的定义
(add1 '67)

(define (sub1 n); sub1函数是指在参数的值上减一
  (- n 1)); 给出sub1的定义
(sub1 5)
(sub1 0); p59

; p60
(zero? 0)
(zero? 1942)
(+ 46 12)
(define (++ n m)
 (cond
  ((zero? m) n); 当m为0时，n作为返回结果,要有终止条件
  (else (add1 (++ n (sub1 m)))))); 本质上就是每递归一次就在n的基础上加一
  ; cons构建列表，add1构建数字
(++ 46 12)

; p61
(- 14 3)
(- 17 9)
(define (-- n m)
  (cond
    ((zero? m) n); 这是终止条件
    (else (sub1 (-- n (sub1 m)))))); 本质上就是每递归一次就在n的基础上减一
(- 18 25)

; p64
(define (addup tup)
  (cond
    ((null? tup) 0)
    (else (+ (car tup) (addup (cdr tup)))))); addup函数是将tup列表中的所有数字相加
(addup '(12 23 34))

; p65
(define (** n m)
 (cond
  ((zero? m) 0); 当m为0时，是递归终止条件
  ((eq? 1 m) n)  (else (+ n (** n (sub1 m)))))); 这里的本质上是有m个n相加，每一次递归n就会加一次n,m的个数决定相加几次
(** 2 3)

; p68,tup可为空列表
#; (define (tup+ tup1 tup2); 这里的tup+函数是针对于相同长度的列表tup1、tup2，将tup1的第一个数字加到tup2的第一个数字上，后面数字以此类推，形成一个新列表
    (cond
     ((and (null? tup1) (null? tup2)) '()); 为了确定tup1和tup2长度一致
     (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))))
    ; 先将tup1的第一个数字加到tup2的第一个数字上，尔后再用cons将刚所得数字加入递归所得的列表中

;(tup+ '(1 2 3) '(1 2 3))

; p71
(define (tup+ tup1 tup2); 这是经过修改的tup+函数，这里的两个参数列表长度可以不同
 (cond
  ((null? tup1) tup2); 当tup1的长度小于tup2时，返回tup2列表
  ((null? tup2) tup1); 当tup2的长度小于tup1时,返回tup1列表
  (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))))
(tup+ '(1 2 3 4) '(1 2 3))

; p72
(define (> n m); 比较n是否大于m
  (cond
    ((zero? n) #f) 
    ((zero? m) #t); 若此行代码作为第一个条件就会出现(> 5 5)返回值为#t的错误情况
    ((and (zero? n) (zero? m)) #f)
    ; 这里要考虑一个因素m和n都为0时，要返回#f,但是有没有上面这行代码都不会影响，因为已经确保((zero? n) #f)为第一个条件
    (else (> (sub1 n) (sub1 m)))))
(> 5 5)
(> 12 23)

; p73
(define (< n m); 比较n是否小于m
  (cond
    ((zero? n) #f) 
    ((zero? m) #t)
    (else (> (sub1 n) (sub1 m)))))

; p74
#;(define (= n m)
  (cond
    ((zero? n) (zero? m)); 这段代码看不懂
    ((zero? n) #f)
    (else (= (sub1 n) (sub1 m)))))

 (define (= n m); 判断两个参数是否相等
   (cond
     ((> n m) #f)
     ((< n m) #f)
     (else #t)))

(define (power n m); 求n的m次方
  (cond
    ((zero? m) 1); 这是终止条件
    (else (* n (power n (sub1 m)))))); 其原理为递归几次，n就乘以几次n
(power 2 3)

; p75
