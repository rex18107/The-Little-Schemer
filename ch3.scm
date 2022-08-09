#lang scheme

;p34
#| （define rember
     (lambda (a lat)
      (cond
       ((null? lat) '())
       ((eq? a (car lat)) (cdr lat))
      (else
       (rember a (cdr lat)|#

;上部分代码是不完整的，代码的结果很可能会缺少(car lat)及其函数递归后的（car lat）

;p37
(define rember
   (lambda (a lat)
     (cond
       ((null? lat) '())
       ((eq? a (car lat)) (cdr lat))
     (else
       (cons (car lat) (rember a (cdr lat)))))))
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

;p47
(insertR 'topping 'fudge '(ice cream with fudge dessert))
(insertR 'jalapeno 'and '(taco tamales and salsa))

;p48
(insertR 'e 'd '(a b c d f d h));insertR函数只会在第一个old后面插入new
(insertR 'e 'f '(a b c d f d h))

;p51
(define (insertL new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new lat))
    (else (cons (car lat) (insertL new old (cdr lat))))))
 (insertL 'a 'b '(b c d))
 (insertL 'a 'b '(b c d a))

(define (subst new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new (cdr lat)))
    (else (cons (car lat) (subst new old (cdr lat))))))
(subst 'h 'd '(a b c d e));test

;p52
(define (subst2 new o1 o2 lat)
  (cond
    ((null? lat) '())
    ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
    (else (cons (car lat) (subst2 new o1 o2 (cdr lat))))))
(subst2 'apple 'carrot 'onion '(onion soup is better than carrot soup));test

;p53
(define (multirember a lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat)  a) (multirember a (cdr lat)))
    (else (cons (car lat) (multirember a (cdr lat))))))
(multirember 'cup '(coffee cup tea cup and hick cup))

;p56
(define (multiinsertR new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
    (else (cons (car lat) (multiinsertR new old (cdr lat))))))
(multiinsertR 'hi 'hello '(you hello I he hello));test

;p57
(define (multiinsertL new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new (cons old(multiinsertL new old (cdr lat)))))
    (else (cons (car lat) (multiinsertL new old (cdr lat))))))
(multiinsertL 'hi 'hello '(hello you I he hello));test

(define (multisubst new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
    (else (cons (car lat) (multisubst new old (cdr lat))))))
(multisubst 'h 'd '(a d b c d e));test
