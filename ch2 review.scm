
; 1.(member? 1 `(1,2,3)) 是t还是f？为什么？
; Ans：
; #t  ,忽略

; (member? 1 `(123)) 是t还是f？为什么？
; Ans：
; #f ；1不是list(123)的元素

; (member? 1 `(1 2 3)) 是t还是f？为什么？
; Ans：
; #t  ；1是list（1 2 3）的元素

; (member? (1) `(1 2 3)) 是什么？  为什么？
; Ans：
; 不定义(1)会报错；'(1)后无论member第二个参数列表是'(1 2 3)或是'((1) 2 3)答案都是#f
; ；member的第一个参数似乎只能是原子

; 2.实现一个函数，接受一个数值n，，计算1 + 2 + 3 + ..n的求和

; 例 n = 10 得 55 （填空)
; n = 100 的 5050
; Ans1（填空）：

(define (sum n)
  (cond ((eq? n 1) 1)
        (else (+ n (sum (- n 1))))))

; Ans2（这是我在没看填空的情况先完成的，但是用到了后面章节看到过的zero：
(define sum
  (lambda (n)
    (cond
      ((zero? n) 0)
      (else (+ n (sum (- n 1)))))))


; 3.实现一个函数，接受含有n个数字的列表，计算他们相乘是多少
; 如1 * 2 * 3 * 74 = 444
; Ans：
(define (product_all l)
  (cond
    ((null? l) 1)
    (else (* (car l) (product_all (cdr l))))))

; 4.实现一个函数，比较给出的列表中是否含有数字44？
; 输出：
; (find_44 `(34 44 11)) ;#t
; (find_44 `(34 11)) ;#f
Ans：
(define (find_44 l)
  (cond
    ((null? l) #f)
    (else (or (eq? (car l) 44) (find_44 (cdr l))))))

; 以下是奈奈的做法
(define (find_44 list)
  (cond ((null? list) #f)
        ((eq? (car list) 44) #t)
        (else (find_44 (cdr list)))))
; 奈奈的做法更好，可以省略or这一步但是不太清楚，增加or主要是影响内存还是时间


; 5.可选挑战题：

; 实现一个函数，比较给出的列表listA和listB是否完全一样？

; 如(1 2 3) (1 2 3)就完全一样 输出#t
; 如果(4 5 1) (1 2 3)就不一样 输出#f
; Ans：
(define (compare_list A B)
  (cond
    ((null? A)
     (cond
       ; 先确认两个参数元素个数是相同的
       ((null? B) #t)    
       (else #f)))
    ; 这个是为了避免出现当参数为A '(1 2 3 4),B '(1 2 3)时,出现报错
    ((null? B) #f)       
    (else (cond
            ((eq? (car A) (car B)) (compare_list (cdr A) (cdr B)))
            (else #f)))))
