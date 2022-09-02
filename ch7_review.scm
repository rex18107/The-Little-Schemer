#lang scheme

;完成以下函数
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; pair的第一个元素
(define (first p)
  (car p))
; pair的第二个元素
(define (second p)
  (car (cdr p)))

#|
1.call: 传入函数func 和输入值 input，输出func对应的输出数值
这里的func指的是这一章里面提到的，由pair构成并且每个pair的第一个元素都是唯一的列表。
|#

(define (call func input)
  (cond
    ; 如果input与func中列表元素的第一个原子相同，则返回此列表的第二个元素
    ((eq? input (first (car func)))
     (second (car func)))
    ; 若第一个列表元素的第一个原子不是对应的input，就找func的第二个列表元素，进行递归
    (else (call (cdr func) input))))

  

(call '((8 3) (4 5) (1 a)) 8); 3
(call '((8 3) (4 5) (1 a)) 1); a



#|
2.eval: 一个可以执行并集和交集操作的表达式运算器
如：(1 2) ∪ (2 3) 得到(1 2 3)
(1 2) ∩ (2 3) 得到(2)
((1 2) ∪ (2 3)) ∩ (2 3) 得到 (2 3)
|#

; 判断a 是否为列表l的元素
(define (member? a l)
  (cond
   ((null? l) #f)
   ((eq? (car l) a) #t)
   (else (member? a (cdr l)))))

; 一个算术表达式的表示方式中的第一个子表达式
(define (1st-sub-exp aexp)
  (car aexp))
; 一个算术表达式的表示方式中的第二个子表达式
(define (2nd-sub-exp aexp)
  (car (cdr (cdr aexp))))
; 表达操作符所在位置
(define (operator aexp)
  (car (cdr aexp)))

; 判断列表是否全为原子
(define (lat? n)
  (cond
    ((null? n) #t)
    ((atom? (car n)) (lat? (cdr n)))
    (else #f)))
  

; 求并集
(define (∪ s1 s2)
  (cond
    ((null? s1) s2)
    ; 将s1中存在于s2中的元素去除,避免重复出现在构筑的新列表中
    ((member? (car s1) s2)
     (∪ (cdr s1) s2))
    (else
     (cons (car s1) (∪ (cdr s1) s2)))))

; (1 4 2 3)
(∪ '(1 2 4) '(2 3))

; 求交集
(define (∩ s1 s2)
  (cond
    ; 表示s1中元素已经比较完,且没有一个原子存在于s2中
    ((null? s1) '())
    ; 调用member函数判断s1的第一个函数是否存在于s2中
    ((member? (car s1) s2)
     ; 存在就构筑在调用函数递归所得的新列表中 
     (cons (car s1) (∩ (cdr s1) s2)))
    (else (∩ (cdr s1) s2))))

; 一个可以执行并集和交集操作的表达式运算器
(define (eval nexp)
 (cond
   ; 判断参数是否为原子列表
   ((lat? nexp) nexp)
   ; 如果参数的第二个元素是∪，就调用求并集函数
   ((eq? (operator nexp) '∪)
    (∪ (eval (1st-sub-exp nexp))
       (eval (2nd-sub-exp nexp))))
   ; 如果参数的第二个元素是∩,就调用求交集函数
   (else (eq? (operator nexp) '∩)
    (∩ (eval (1st-sub-exp nexp))
       (eval (2nd-sub-exp nexp))))))

; (1 4 2 3)
(eval '((2 7 1) ∪ (1 2 4 3)))
; (4)
(eval '(((3 4 1) ∩ (2 3 4)) ∩ (4 5)))
; (4 2 3)
(eval '(((1 2 4) ∪ (2 3)) ∩ (2 3 4)))
