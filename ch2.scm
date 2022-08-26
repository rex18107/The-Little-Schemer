#lang scheme
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; p16
; 每个lat都是原子组成的列表,lat可以为空列表
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))  

; p15
(let ((l '(Jack Sprat could eat no chicken))) (lat? l))
(let ((l '((Jack) Sprat could eat no chicken))) (lat? l))
(let ((l '(Jack (Sprat could) eat no chicken))) (lat? l))
(let ((l '())) (lat? l))

; p16
(let ((l '(bacon and eggs))) (lat? l))

; p21
(let ((l1 '()) (l2 '(d e f g))) (or (null? l1) (atom? l2)))
(let ((l1 '(a b c)) (l2 '())) (or (null? l1) (null? l2)))
(let ((l1 '(a b c)) (l2 '(atom))) (or (null? l1) (null? l2)))

; p22
#|(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))|#
; member函数是判断元素a是否为列表lat里的元素
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat)) (member? a (cdr lat)))))))
(let ((a 'tea) (lat '(coffee tea or milk))) (member? a (cdr lat)))
(let ((a 'poached) (lat '(fried eggs and scrambled eggs))) (member? a (cdr lat)))

; p28
(let ((a 'liver) (lat '(bagels and lox))) (member? a lat))
