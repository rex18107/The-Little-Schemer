#lang scheme

;完成以下函数
(define (sub1 n); sub1函数是指在参数的值上减一
  (- n 1))

;<<:  二进制位运算左移,如输入列表(0 0 1 1) 左移 1 次以后就是(0 1 1 0) 左移 3 次是(1 0 0 0)

(define rember   ; rember函数是移除列表lst中的a元素
     (lambda (a lat)
      (cond
       ((null? lat) '())
       ((eq? a (car lat)) (cdr lat))
      (else
       (rember a (cdr lat))))))
(define (add_0_to_right lst)  ; 在列表lst的最右边一个元素后加上0
  (cond
    ((null? lst) (cons 0 lst)); 找到列表最右边的元素加0，这是结束递归的条件
    (else (cons (car lst) (add_0_to_right (cdr lst))))))
   (add_0_to_right '(1 0 1 1))
(define (<< lst n)
  (cond
    ((zero? n) lst)
    (else  (<< (rember (car lst) (add_0_to_right lst)) (sub1 n)))))   ; 调用了两个函数进行嵌套递归
(<< '(1 0 1 1) 1)
(<< '(0 0 1 1) 1)
(<< '(0 0 1 1) 2)

;(<< '(1 0 1 1) 1) #(0 1 1 0)
;(<< '(0 0 1 1) 2) #(1 1 0 0)

;&:  二进制位运算与操作,输入两个列表，返回一个新列表，只有A和B列表中都为1的位才会出现在新列表中显示1否则是0
 
(define (one? n) ; 判断n是否为1
  (cond
    (else (zero? (sub1 n)))))
(define (multisubst new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
    (else (cons (car lat) (multisubst new old (cdr lat))))))
(define (& lst1 lst2)
  (cond
    ((null? lst1) (multisubst 0 1 lst2)); 当lst1的长度比lst2短，用0来替换lst2多于lst1的其它元素
    ((null? lst2) (multisubst 0 1 lst1))
    ((and (one? (car lst1))  (one? (car lst2))) (cons 1 (& (cdr lst1) (cdr lst2))))
    (else (cons 0 (& (cdr lst1) (cdr lst2))))))
 
  
(& '(1 0 1 1 0 0 0 1) '(0 0 1 1 0 1 0 1)) ;#(0 0 1 1 0 0 0 1)
(& '(1 0 1) '(0 1 1 1 1)); (0 0 1 0 0)


;binary to decimal: 将二进制列表转为整数，如(0 0) 就是 0， (0 1) 就是1 (1 1)就是2
;提示：1011对应2^3 + 2^1 + 2^0 = 11 以此类推。 
;利用前面写的位运算来判断某一位比特是否是1
#;
(define (binary_to_decimal lst)
  (cond
    


;(binary_to_decimal '(1 0 1 1)) #11
;(binary_to_decimal '(0 0 1 1)) #3

;binary addition： 得到一个二进制列表，返回这个二进制列表 + 1的数 如(0 0 1) 返回 (0 1 0), (1 0 1 1) 返回 (1 1 0 0)
#;
(define (binary_addition lst))
 
;(binary_addition '(1 0 1 1)) # (1 1 0 0)
;(binary_addition '(0 0 1 1)) # (1 0 0)
;(binary_addition '(1 1 1 1)) # (1 0 0 0 0)