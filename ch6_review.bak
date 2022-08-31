#lang scheme

;完成以下函数
;注意：这是基于原本功能增加新功能，以下增加的功能不可影响旧功能的使用！！
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(define (^ n m)
  (cond
    ((zero? m) 1)
    (else (* n (* n (- m 1))))))
; 一个算术表达式的表示方式中的第一个子表达式
(define (1st-sub-exp aexp)
  (car aexp))
; 一个算术表达式的表示方式中的第二个子表达式
(define (2nd-sub-exp aexp)
  (car (cdr (cdr aexp))))
; 表达操作符所在位置
(define (operator aexp)
  (car (cdr aexp)))
#|
1.value:  增加value的功能，可以支持坐标相加，也就是支持有2个数字的列表，分别代表x y，相加以后得到(x1+x2 y1+y2)的结果。
如(1 2) + (3 4) = (4 6)
提示：我们需要重新定义 + 流程并且修改value的代码？
|#
; 一个参数求出列表元素相加后构成的新列表
(define (value-list nexp)
  (cond
    ((atom? nexp) nexp)
    ; 将两个列表的第一个元素相加之后构筑到两个列表的第二个元素相加后的值上，再构筑到空列表中
    (else (cons
           (+ (value-list (car (car nexp)))
              (value-list (car (car (cdr (cdr nexp))))))
           (cons
            (+ (value-list (car (cdr (car nexp))))
              (value-list (car(cdr (car (cdr (cdr nexp)))))))
            '())))))
; 两个参数求出列表元素相加后构成的新列表
(define (v-list n m)
  (cond
    ((or (null? n) (null? m)) '())
    (else
     (cons
      (+ (car n) (car m))
         (v-list (cdr n) (cdr m))))))
; (3 5)
(v-list '(1 2) '(2 3))
; (4 6)
(value-list '((1 2) + (3 4)))

(define (value-v2 nexp)
 (cond
   ((atom? nexp) nexp)
    ; 当nexp值为列表时,调用value-list函数
   ((list? (car nexp))
    (v-list (1st-sub-exp nexp)
            (2nd-sub-exp nexp)))
   ((eq? (operator nexp) '+)
     (+ (value-v2 (1st-sub-exp nexp))
      (value-v2 (2nd-sub-exp nexp))))
   ((eq? (operator nexp) '*)
     (* (value-v2 (1st-sub-exp nexp))
      (value-v2 (2nd-sub-exp nexp))))
   (else
    (eq? (operator nexp) '^)
     (^ (value-v2 (1st-sub-exp nexp))
      (value-v2 (2nd-sub-exp nexp))))))
; (4 6)
(value-v2 '((1 2) + (3 4)))

#|
value:  增加value的功能,可以支持点积。 如  a ⋅ b 当 a = (3 5 8) 且 b = (2 7 1)，那么：

a ⋅ b = (a1 * b1) + (a2 * b2) + (a3 * b3)
a ⋅ b = (3 * 2) + (5 * 7) + (8 * 1)
a ⋅ b = 6 + 35 + 8
a ⋅ b = 49
提示：我们需要定义一个 点积 流程并且修改value的代码？
|#
; 只用一个参数计算点积结果
(define (value-d nexp)
  (cond
    ; 使递归最后一步为（（8）⋅（1））的形式，让8*1
    ((null? (cdr(car nexp)))
     (*  (car (car nexp))
            (car (car (cdr (cdr nexp))))))
    ((atom? nexp) nexp)
    ; 将两个列表的第一个元素积之后加到两个列表的第二个元素积后的值上
    (else (+ (* (car (car nexp))
                (car (car (cdr (cdr nexp)))))
             ; 递归调用y跟z的值
             (value-d (cons (cdr (car nexp))
                            (cons '⋅
                                  (cons
                                   (cdr (car (cdr (cdr nexp))))
                                   '()))))))))
; 49
(value-d '((3 5 8) ⋅ (2 7 1)))
; 引入两个参数定义点积的算法
(define (⋅ n m)
  (cond 
   ((or (null? n) (null? m)) 0)
   (else (+ (* (car n) (car m))
      (⋅ (cdr n) (cdr m)))))) 
; 21
(⋅ '(3 2 8) '(3 2 1))

; 此函数可以兼容原子与列表的加法,只考虑列表中元素个数相同的情况
(define (++ n m)
  (cond
    ; n,m都为原子的话直接相加
    ((and (atom? n) (atom? m)) (+ n m))
    (else (cond
            ((or (null? n) (null? m)) '())
            ; n，m为列表的情况
            (else
             (cons
              (+ (car n) (car m))
              (++ (cdr n) (cdr m))))))))
; 11
(++ 2 9)
; (3 5 7)
(++ '(1 2 3) '(2 3 4))

; 判断是否列表中只含数字,可以有列表元素。但列表元素也只含数字
(define  (digit-only? aexp)
  (if
   (atom? aexp)
   (number? aexp)
   (and (digit-only? (car aexp))
        (digit-only? (car (cdr aexp))))))
; #t
(digit-only? '(2 (1 3 (2 3)) 1))
; #f
(digit-only? '(1 + 2))

; 这里函数的参数形式为（a 运算符 b），规定了a和b的三种形式
; 数字，只含数字的列表，
(define (value-v3 nexp)
  (cond
    ((atom? nexp) nexp)
     ; 可以解决调用value-v3递归后的元素中含有列表的情况
    ((digit-only? nexp) nexp)
    ((eq? '+ (operator nexp))
      ; 这里同时兼容了原子和列表的加法
     (++ (value-v3 (1st-sub-exp nexp))
         (value-v3 (2nd-sub-exp nexp))))
    ((eq? '* (operator nexp))
     (* (value-v3 (1st-sub-exp nexp))
         (value-v3 (2nd-sub-exp nexp))))
    ((eq? '^ (operator nexp))
     (^ (value-v3 (1st-sub-exp nexp))
         (value-v3 (2nd-sub-exp nexp))))
     ; 计算nexp值为点积列表的值
    ((eq? '⋅ (operator nexp))
     (⋅ (value-v3 (1st-sub-exp nexp))
         (value-v3 (2nd-sub-exp nexp))))))

; 8 
;(value-v3 '(3 * 5))
; (5 5 10 2)
;(value-v3 '((3 5 3 1) + (2 0 7 1)))
; 28        
(value-v3 '((3 5 3 1) ⋅ (2 0 7 1)))
(value-v3 '(1 + (2 + 3)))
(value-v3 '((1 2) + ((1 1) + (1 1))))
(value-v3 '((3 5 3 1) ⋅ (2 ((3 3) ⋅ (1 2)) 7 1)))
