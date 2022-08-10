#lang scheme

;p34
;(define rember
     (lambda (a lat)
      (cond
       ((null? lat) '())
       ((eq? a (car lat)) (cdr lat))
      (else
       (rember a (cdr lat)
;rember函数是去除列表lat中的第一个a原子
;上部分代码是不完整的，代码的结果很可能会缺少(car lat)及其函数递归后的（car lat）

;p37
(define rember
   (lambda (a lat)
     (cond
       ((null? lat) '())
       ((eq? a (car lat)) (cdr lat))
     (else
       (cons (car lat) (rember a (cdr lat)))))))
;rember函数是去除列表lat中的第一个a原子，构成一个新列表
;上为修改后的rember函数

;p33
(let ((a 'toast) (lat '(bacon lettuce and tomato))) (rember a lat))
(let ((a 'cup) (lat '(coffee cup tea cup and hick cup))) (rember a lat))

;p42
(rember 'sauce '(soy sauce and tomato sauce))



(define (firsts l)
  (cond
    ((null? l) '())
    (else (cons (car (car l)) (firsts (cdr l))))));p44
(firsts '((apple red) (banana yellow) (grape purple))); test
;first函数是取出列表l中的每个列表元素里的第一个元素，构成一个新列表

;p43
(firsts '((a b) (c d) (e f)))
(firsts '())
(firsts '((five plums) (four) (eleven green oranges)))
(firsts '(((five plums) four) (eleven green oranges)))

;p48
(define (insertR new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons old (cons new (cdr lat))))
    (else (cons (car lat) (insertR new old (cdr lat))))))
;insertR函数是将new元素插入列表lat中第一个old元素的右边，构成一个新列表

;p47
(insertR 'topping 'fudge '(ice cream with fudge dessert))
(insertR 'jalapeno 'and '(taco tamales and salsa))

;p48
(insertR '(e) 'd '(a b c d f d h));insertR函数只会在第一个old后面插入new
(insertR 'e 'f '(a b c d f d h))

;p51
(define (insertL new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new lat))
    (else (cons (car lat) (insertL new old (cdr lat))))))
 (insertL 'a 'b '(b c d))
 (insertL 'a 'b '(b c d a))
;insertL函数是将new元素插入列表lat中第一个old元素的左边，构成一个新列表

(define (subst new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new (cdr lat)))
    (else (cons (car lat) (subst new old (cdr lat))))))
;subst函数是用new元素替换掉列表lat中的第一个old元素，构成一个新列表
(subst 'h 'd '(a b c d e));test


;p52
(define (subst2 new o1 o2 lat)
  (cond
    ((null? lat) '())
    ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
    (else (cons (car lat) (subst2 new o1 o2 (cdr lat))))))
;subst2函数是用new元素替换掉列表lat中的第一个出现o1元素或是第一个出现的o2元素，构成一个新列表
(subst2 'apple 'carrot 'onion '(onion soup is better than carrot soup));test

;p53
(define (multirember a lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat)  a) (multirember a (cdr lat)))
    (else (cons (car lat) (multirember a (cdr lat))))))
;multirember函数是去除列表lat中的每一个a原子，构成一个新列表
(multirember 'cup '(coffee cup tea cup and hick cup))

;p56
(define (multiinsertR new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
    (else (cons (car lat) (multiinsertR new old (cdr lat))))))
;multiinsertR函数是将new元素插入列表lat中每一个old元素的右边，构成一个新列表
(multiinsertR 'hi 'hello '(you hello I he hello));test

;p57
(define (multiinsertL new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new (cons old(multiinsertL new old (cdr lat)))))
    (else (cons (car lat) (multiinsertL new old (cdr lat))))))
;multiinsertL函数是将new元素插入列表lat中每一个old元素的左边，构成一个新列表
(multiinsertL 'hi 'hello '(hello you I he hello));test

(define (multisubst new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
    (else (cons (car lat) (multisubst new old (cdr lat))))))
;subst函数是用new元素替换掉列表lat中的每一个old元素，构成一个新列表
(multisubst 'h 'd '(a d b c d e));test
