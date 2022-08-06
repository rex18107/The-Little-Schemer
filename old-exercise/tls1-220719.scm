#lang scheme
(define l '((b)c d))
(define a 'peanut)
(define e 'w)
(define y '(butter and jelly))
(define s '(banana and))
(define x '(peanut butter and jelly))
(cons s x)
(cons a e)
;cons 有两个参数，第二个参数必须是列表，结果也是一个列表
(cons e (car l))
(cdr(cons e (car l)));为什么结果为（w b）
(car(cons e (car l)))


