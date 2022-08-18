#lang scheme
(define (power_of_2 n); 求取2的n次方
  (cond
   ((zero? n) 1)
   (else (* 2 (power_of_2 (sub1 n))))))
(define (ets lis n s)
(cond 
((null? lis) s)
(#t (ets (cdr lis) (+ n 1) 
(if (= 1 (car lis)) (+ s (power_of_2 n))  s)))))
(ets '(1 0 1 1) 0 0 )
#;(def (ets lis n s)
(cond 
((null? lis) s)
(#t (ets (cdr lis) (+ n 1) 
(if (= 1 (car lis)) (+ s (pow 2 n))  s)))))

(define (pow n m)
  (cond
    ((= m 0) 1)
    (else (* n (pow n (- m 1))))))
(pow 2 3)
#;(define (delete_right lst); 删除最右边的元素后返回列表
  (cond 
    ((null? (cdr lst)) '()); 此结束条件做到了删除最右边的元素
    (else (cons (car lst) (delete_right (cdr lst))))))

;(let ((l '())) (cons 1 (cdr (delete_right l))))
