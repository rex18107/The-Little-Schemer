#lang scheme


#| （define rember
     (lambda (a lat)
      (cond
       ((null? lat) '())
       ((eq? a (car lat)) (cdr lat))
      (else
       (rember a (cdr lat)|#
;p34
;上部分代码是不完整的，代码的结果很可能会缺少(car lat)及其函数递归后的（car lat）


(define rember
   (lambda (a lat)
     (cond
       ((null? lat) '())
       ((eq? a (car lat)) (cdr lat))
     (else
       (cons (car lat) (rember a (cdr lat)))))))
;p37,上为修改后的rember函数

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

