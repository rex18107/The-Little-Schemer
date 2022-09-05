#lang scheme

; p34
; rember函数是去除列表lat中的第一个a原子
; 此部分代码是不完整的,代码的结果很可能会缺少(car lat)及其函数递归后的(car lat)
#;
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ; 判断列表的第一个元素是否等于a，是的话就返回（cdr lat）
      ((eq? a (car lat)) (cdr lat))
      (else
       (rember a (cdr lat))))))

; p37
; 此为修改后的rember函数
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else
       ; 列表的第一个元素不等于a,加上第一个元素对列表其余元素继续进行递归调用
       (cons (car lat) (rember a (cdr lat)))))))

; p33
(let ((a 'toast) (lat '(bacon lettuce and tomato))) (rember a lat)); (bacon lettuce and tomato)
(let ((a 'cup) (lat '(coffee cup tea cup and hick cup))) (rember a lat)); (coffee tea cup and hick cup)

; p42
(rember 'sauce '(soy sauce and tomato sauce)); (soy and tomato sauce)
; first函数是取出列表l中的每个列表元素里的第一个元素,构成一个新列表
(define (firsts l)
  (cond
    ((null? l) '())
    (else (cons (car (car l)) (firsts (cdr l))))))
; test
(firsts '((apple red) (banana yellow) (grape purple))); (apple banana grape)

; p43
(firsts '((a b) (c d) (e f))); (a c e)
(firsts '()); ()
(firsts '((five plums) (four) (eleven green oranges))); (five four eleven)
(firsts '(((five plums) four) (eleven green oranges))); ((five plums) eleven)

; p48
; insertR函数是将new元素插入列表lat中第一个old元素的右边,构成一个新列表
(define (insertR new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons old (cons new (cdr lat))))
    (else (cons (car lat) (insertR new old (cdr lat))))))

; p47
(insertR 'topping 'fudge '(ice cream with fudge dessert)); (ice cream with fudge topping dessert)
(insertR 'jalapeno 'and '(taco tamales and salsa)); (taco tamales and jalapeno salsa)

; p48
; insertR函数只会在第一个old后面插入new
(insertR '(e) 'd '(a b c d f d h)); (a b c d (e) f d h)
(insertR 'e 'f '(a b c d f d h)); (a b c d f e d h)

; p51
; insertL函数是将new元素插入列表lat中第一个old元素的左边,构成一个新列表
(define (insertL new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new lat))
    (else (cons (car lat) (insertL new old (cdr lat))))))
(insertL 'a 'b '(b c d)); (a b c d)
(insertL 'a 'b '(b c d a)); (a b c d a)
; subst函数是用new元素替换掉列表lat中的第一个old元素,构成一个新列表
(define (subst new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new (cdr lat)))
    (else (cons (car lat) (subst new old (cdr lat))))))
(subst 'h 'd '(a b c d e)); (a b c h e)


; p52
; subst2函数是用new元素替换掉列表lat中的第一个出现o1元素或是第一个出现的o2元素,构成一个新列表
(define (subst2 new o1 o2 lat)
  (cond
    ((null? lat) '())
    ; 判断列表第一个元素是不是等于o1或o2
    ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
    (else (cons (car lat) (subst2 new o1 o2 (cdr lat))))))
(subst2 'apple 'carrot 'onion '(onion soup is better than carrot soup)); (apple soup is better than carrot soup)

; p53
; multirember函数是去除列表lat中的每一个a原子,构成一个新列表
(define (multirember a lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat)  a) (multirember a (cdr lat)))
    (else (cons (car lat) (multirember a (cdr lat))))))
(multirember 'cup '(coffee cup tea cup and hick cup)); (coffee tea and hick)

; p56
; multiinsertR函数是将new元素插入列表lat中每一个old元素的右边,构成一个新列表
(define (multiinsertR new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
    (else (cons (car lat) (multiinsertR new old (cdr lat))))))
(multiinsertR 'hi 'hello '(you hello I he hello)); (you hello hi I he hello hi)

; p57
; multiinsertL函数是将new元素插入列表lat中每一个old元素的左边,构成一个新列表
(define (multiinsertL new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new (cons old(multiinsertL new old (cdr lat)))))
    (else (cons (car lat) (multiinsertL new old (cdr lat))))))
(multiinsertL 'hi 'hello '(hello you I he hello)); (hi hello you I he hi hello)
; multisubst函数是用new元素替换掉列表lat中的每一个old元素,构成一个新列表
(define (multisubst new old lat); (a h b c h e)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
    (else (cons (car lat) (multisubst new old (cdr lat))))))
(multisubst 'h 'd '(a d b c d e));test
