#lang scheme
(null? '())
(define l '(a b c))
(null? l)
(define a 'spaghetti)
(null? a);null只针对列表
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(atom? a)
(define s '(cat is so cute))
(atom? s);atom只有一个参数
(define b '(cat dog))
(cdr b)
(car b)