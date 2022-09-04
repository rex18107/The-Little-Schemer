#lang scheme

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; 求n的m次幂
(define (^ n m)
  (cond
    ((zero? m) 1)
    (else (* n (* n (- m 1))))))
; 判断两个列表是否相等
(define (eqlist? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    ((and (atom? (car l1)) (atom? (car l2)))
     (and (eq? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2))))
    ((or (atom? (car l1)) (atom? (car l2))) #f)
    (else
     ; 这里比eqlist?-v1这个版本胜在直接调用递归,简化了步骤,代码更简单
     (and (eqlist? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2))))))
; 判断两个S-表达式是否相同
(define (equal? s1 s2)
  (cond
    ; 如果两个参数都为原子的话,判断原子是否相同,返回其值即可
    ((and (atom? s1) (atom? s2))
     (eq? s1 s2))
    ; 若其中一个参数为原子,另外一个参数肯定为列表,
    ; 因为上面一行代码已经排除了同时为原子的情况,才进入到这行代码的,所以返回#f
    ((or (atom? s1) (atom? s2)) #f)
    ; 此为两个参数都为列表的情况,则调用eqlist函数比较两参数是否相同
    (else (eqlist? s1 s2))))

; 將l中第一第一个出现的a删除
; p126
(define (rember-f test? a l)
  (cond
    ((null? l) '())
    ((test? (car l) a) (cdr l))
    (else
     (cons (car l)
           (rember-f test? a (cdr l))))))
; (d s e)
(rember-f equal? 'a '(d a s e))

; p117
#| 这是一个函数，当传给它一个参数a时，它会返回一个函数
   (lambda (x)
   (eq? x a))
   这里前面的a就是前面传入的参数a
   ，这叫做“Curry-ing” 柯里化|#
#;(lambda (a)
  (lambda (x)
    (eq? x a)))

; 测试函数能够返回的类型值--函数
(define (eq?-c a)
  (lambda (x)
    (eq? x a)))
; #f
((eq?-c 'a) 'b)

; 这里有三个参数，test？，a，l
; p129
(define ((rember-f-v2 test?) a l)
  (cond
    ((null? l) '())
    ((test? (car l) a) (cdr l))
    (else (cons (car l)
                ((rember-f-v2 eq?) a (cdr l))))))
; (d c w)
((rember-f-v2 eq?) 'a '(d a c w))

; rember-f-v2跟rember-f-v3的优缺点？
(define (rember-f-v3 test? a l)
  (cond
    ((null? l) '())
    ((test? (car l) a) (cdr l))
    (else (cons (car l)
                (rember-f-v3 test? a (cdr l))))))
(rember-f-v3 eq? 'a '(d a c w))

; p130
; 将insertL变成insertL-f
(define ((insetL-f test?) new old l)
  (cond
    ((null? l) '())
    ((test? (car l) old)
     (cons new l))
    (else
     (cons (car l)
          ((insetL-f test?) new old (cdr l))))))
; (s d h a f)
((insetL-f eq?) 'h 'a '(s d a f ))

; 将insertR变成insertR-f
(define ((insetR-f test?) new old l)
  (cond
    ((null? l) '())
    ((test? (car l) old)
     (cons old
           (cons new (cdr l))))
    (else
     (cons (car l)
           ((insetR-f test?) new old (cdr l))))))
; (s d a h f)
((insetR-f eq?) 'h 'a '(s d a f ))

; p131
; 定义一个函数，cons第二个参数到第三个参数上，
; 得到一个cons的结果，然后再把第一个参数cons到这个结果上
(define (seqL new old l)
  (cons new
        (cons old l)))

; 定义一个函数,cons第一个参数到第三个参数上,
; 得到一个cons的结果,然后再把第二个参数cons到这个结果上
(define (seqR new old l)
  (cons old
        (cons new l)))

; p132
; 带有seq的insert-g函数，目的使一个函数拥有insertL和insertR两个功能
(define ((insert-g seq) new old l)
  (cond
    ((null? l) '())
    ((eq? (car l) old)
     (seq new old (cdr l)))
    (else (cons (car l)
                ((insert-g seq) new old (cdr l))))))

; 作为替代将seqL的定义传递到逻辑中，如下
(define insertL2
  (insert-g
   ; 这里是将seqL作为insert-g的参数
   (lambda (new old l)
     (cons new (cons old l)))))
; (s h a d k)
(insertL2 'h 'a '(s a d k))

; 上面代码总结：
; 若多个函数整体逻辑相同，但是代码块中有少量代码行有差异，
; 可以将相同部分代码整理成一个函数A，再将有差异的代码
; 行分别整理为匿名函数（也就是原始代码行），将其根据原来
; 各函数差异，作为A的参数分别带入，以精简代码

; p133
; 用insert-g重新定义subst函数，subst函数是将列表中第一个出现的某元素用其它元素替代
(define subst
  (insert-g
   (lambda (new old l)
     ; 为什么这不是（cons new （cdr l))
     (cons new  l))))
; (d h f)
(subst 'h 'a '(d a f))

#;(define subst
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? (car l) old)
       (cons new (cdr l)))
      (else
       (cons (car l)
             (subst new old (cdr l)))))))
(define (seqs new old l)
  (cons new l))
; (d h b)
((insert-g seqs) 'h 'a '(d a b))

; p134
; 返回函数
(define (atom-to-function x)
  (cond
    ((eq? x '++) +)
    ((eq? x '**) *)
    (else ^)))
; #<procedure:+>
(atom-to-function '+)
; 这个情况不符合参数的类型条件
;(atom-to-function '(++ 5 3))

; 一个算术表达式的表示方式中的第一个子表达式
(define (1st-sub-exp aexp)
  (car (cdr aexp)))
; 一个算术表达式的表示方式中的第二个子表达式
(define (2nd-sub-exp aexp)
  (car (cdr (cdr aexp))))
; 表达操作符所在位置
(define (operator aexp)
  (car aexp))

(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    (else
     ; 先判断是何种运算符，再执行运算
     ((atom-to-function (operator nexp))
      (value (1st-sub-exp nexp))
      (value (2nd-sub-exp nexp))))))
; 3
(value '(++ 1 2))

; p135
; 移除l中每一个a元素,eq?函数用test？作函数参数代替
(define ((multirember-f test?) a l)
  (cond
    ((null? l) '())
    ((test? a (car l))
     ((multirember-f test?) a (cdr l)))
    (else (cons (car l)
                ((multirember-f test?) a (cdr l))))))
; (c c q s l)
((multirember-f eq?) 'a '(c c a q s a l))

; p137
