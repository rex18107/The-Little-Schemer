#lang scheme
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; p97
'a ; a
'+ ; +
'* ; *
(let ((y 'a)) (eq? 'a y)); #t

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
(numbered? '(@ 2));  #t

; numbered函数的简化
(define (numbered-s? aexp)
  (cond
    ((atom? aexp) (number? aexp))
    (else
     (and (numbered? (car aexp))
          (numbered? (car (cdr (cdr aexp))))))))
(numbered-s? '(1 2 2))
; 求n的m次幂
(define (^ n m)
  (cond
    ((zero? m) 1)
    (else (* n (* n (- m 1))))))
(^ 2 3)
; p103
; 求出算术表达式
(define (value nexp)
  (cond
    ; 先判断表达式是不是原子,是的话返回原子作为算术表达式
    ((atom? nexp) nexp)
    ; 判断
    ((eq? (car (cdr nexp)) '+)
     (+ (value (car nexp))
        (value (car (cdr (cdr nexp))))))
    ((eq? (car (cdr nexp)) '*)
     (+ (value (car nexp))
        (value (car (cdr (cdr nexp))))))
    (else
     (^ (value (car nexp))
        (value (car (cdr (cdr nexp))))))))
(value '(2 + (2 ^ 3)))
