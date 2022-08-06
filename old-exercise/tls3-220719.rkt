#lang scheme
(define a1 'Harry)
(define a2 'Harry)
(eq? a1 a2)
(define a3 'Bob)
(eq? a1 a3);eq 有两个参数，并且都为非数字原子
(define a4 '(a))
(define a5 '(a))
(eq? a4 a5);TLS这本书上提到，实际上只要两个列表是相同的，eq?可为true，但是如果如左边的表达式结果，其结果为#f
(define a6 '())
(define a7 '())
(eq? a6 a7);但eq？表达式的参数若为空列表则结果为#t
