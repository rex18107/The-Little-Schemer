#lang scheme
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; p97
; a
'a 
; +
'+ 
; *
'* 
; #t
(let ((y 'a)) (eq? 'a y))

; p100
; 判断一个算术表达式的表示方式中，是否除了+，*,^外只包含数字
(define (numbered? aexp)
  (cond
    ; 先判断表达式是不是原子，是的话再判断是不是数字
    ((atom? aexp) (number? aexp))
    ; 判断(car (cdr aexp))是否为+
    ((eq? (car (cdr aexp)) '+)
     ; 上行代码结果为#t，才可以继续判断+前后位置是否为数字
     (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
    ((eq? (car (cdr aexp)) '*)
     (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
    ((eq? (car (cdr aexp)) '^)
     (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))))
;  #t
(numbered? '(@ 2))

; numbered函数的简化
(define (numbered-s? aexp)
  (cond
    ((atom? aexp) (number? aexp))
    (else
     (and (numbered? (car aexp))
          (numbered? (car (cdr (cdr aexp))))))))
;  #t
(numbered-s? '(1 * 2))
; 求n的m次幂
(define (^ n m)
  (cond
    ((zero? m) 1)
    (else (* n (* n (- m 1))))))
; 8
(^ 2 3)
; p103
; 求出算术表达式
(define (value nexp)
  (cond
    ; 先判断表达式是不是原子,是的话返回原子作为算术表达式
    ((atom? nexp) nexp)
    ; 判断列表第二个元素是否为+,是的话返回第一个元素和第三个元素相加的值
    ((eq? (car (cdr nexp)) '+)
     (+ (value (car nexp))
        (value (car (cdr (cdr nexp))))))
     ; 判断列表第二个元素是否为*,是的话返回第一个元素和第三个元素相乘的值
    ((eq? (car (cdr nexp)) '*)
     (* (value (car nexp))
        (value (car (cdr (cdr nexp))))))
     ; 返回第一个元素的第三个元素值的次方
    (else
     (^ (value (car nexp))
        (value (car (cdr (cdr nexp))))))))
; 10
(value '(2 + (2 ^ 3)))

; p105
; 一个算术表达式的表示方式中的第一个子表达式
(define (1st-sub-exp aexp)
  (car (cdr aexp)))

; p106
; 一个算术表达式的表示方式中的第二个子表达式
(define (2nd-sub-exp aexp)
  (car (cdr (cdr aexp))))

; 将（car nexp）替换成（operator nexp）
(define (operator aexp)
  (car aexp))

; 再定义一次value函数,这也算是规定了算术表达式的表现形式
(define (value-2 nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? (operator nexp) +)
     (+ (value-2 (1st-sub-exp nexp))
        (value-2 (2nd-sub-exp nexp))))
     ((eq? (operator nexp) +)
     (* (value-2 (1st-sub-exp nexp))
        (value-2 (2nd-sub-exp nexp))))
     (else
     (^ (value-2 (1st-sub-exp nexp))
        (value-2 (2nd-sub-exp nexp))))))
; 3
(value-2 (+ 1 2))
; 30
(value-2 (* (+ 2 3) (+ 2 4)))

; p108
; ()代表一个1
(define (sero? n)
  (null? n))
; #t
(sero? '())
; #f
(sero? '(()))

; 加一,用（）表示1
(define (edd1 n)
  (cons '() n))
; (() ())
(edd1 '(()))

; 减一,用()表示1
(define (zub1 n)
  (cdr n))
; (())
(zub1 '(() ()))

; 求出n加m的表示法
(define (++ n m)
  (cond
    ((sero? m) n)
    (else (edd1 (++ n (zub1 m))))))
; (() () () ())
(++ '(()) '(() () ()))

; 判断是否全为原子的列表
(define (lat? l)
  (cond
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f)))
; #f,要小心阴影
(lat? '(() '(() ()) '(() () ())))
