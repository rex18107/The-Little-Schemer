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
#|
1.value:  增加value的功能，可以支持坐标相加，也就是支持有2个数字的列表，分别代表x y，相加以后得到(x1+x2 y1+y2)的结果。
如(1 2) + (3 4) = (4 6)
提示：我们需要重新定义 + 流程并且修改value的代码？
|#
; 求出列表的值
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
(value-list '((1 2) + (3 4)))

(define (value-v2 nexp)
 (cond
   ((atom? nexp) nexp)
   ((eq? (car nexp) '+)
     (+ (value-v2 (car (car nexp)))
      (value-v2 (car (cdr (cdr nexp))))))
   ((eq? (car nexp) '*)
     (* (value-v2 (car (car nexp)))
      (value-v2 (car (cdr (cdr nexp))))))
   ((eq? (car nexp) '^)
     (^ (value-v2 (car (car nexp)))
      (value-v2 (car (cdr (cdr nexp))))))
   ; 当nexp值为列表时，调用value-list函数
   (else  (value-list nexp))))
    

(value-v2 '((1 2) + (3 4))); (4 6)

#|
value:  增加value的功能,可以支持点积。 如  a ⋅ b 当 a = (3 5 8) 且 b = (2 7 1)，那么：

a ⋅ b = (a1 * b1) + (a2 * b2) + (a3 * b3)
a ⋅ b = (3 * 2) + (5 * 7) + (8 * 1)
a ⋅ b = 6 + 35 + 8
a ⋅ b = 49
提示：我们需要定义一个 点积 流程并且修改value的代码？
|#
; 计算点积结果
(define (value-d nexp)
  (cond
    ; 使递归最后一步为（（8）⋅（1））的形式，让8*1
    ((null? (car(car nexp)))
     (* (value-d (car (car nexp)))
              (value-d (car (car (cdr (cdr nexp)))))))
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
(value-d '((3 5 8) ⋅ (2 7 1)))            
           

#;
(define (value-v3 nexp)
 (cond
   ((atom? nexp) nexp)
   ((eq? (car nexp) '+)
     (+ (value-v3 (car (car nexp)))
      (value-v3 (car (cdr (cdr nexp))))))
   ((eq? (car nexp) '*)
     (* (value-v3 (car (car nexp)))
      (value-v3 (car (cdr (cdr nexp))))))
   ((eq? (car nexp) '^)
     (^ (value-v3 (car (car nexp)))
      (value-v3 (car (cdr (cdr nexp))))))
   ; 当nexp值为点积列表时,调用value-d
   (else  (eq? (car (cdr nexp)) '⋅)
          (value-d nexp))))
          

;(value_v3 '((3 5 8) ⋅ (2 7 1))); 49
;(value_v3 '((3 5 3 1) ⋅ (2 0 7 1))); 28
