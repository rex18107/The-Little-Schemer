#lang scheme

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
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
; (s e (a w c))
(rember-f equal? 'a '(a s e (a w c)))

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
