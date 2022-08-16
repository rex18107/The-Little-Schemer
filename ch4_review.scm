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
    (else (cons (car lst)
                (add_0_to_right (cdr lst))))))
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
(define (pick_right lst); 取最右边的元素
  (cond
    ((null? (cdr lst)) (car lst)); 其结束条件为此列表递归至只剩一个元素在列表中
    (else (pick_right (cdr lst)))))
(define (add_1_to_right lst)  ; 在列表lst的最右边一个元素后加上1
  (cond
    ((null? lst) (cons 1 lst)); 找到列表最右边的元素加1,这是结束递归的条件
    (else (cons (car lst) (add_1_to_right (cdr lst))))))
(define (delete_right lst); 删除最右边的元素后返回列表
  (cond 
    ((null? (cdr lst)) '()); 此结束条件做到了删除最右边的元素
    (else (cons (car lst) (delete_right (cdr lst))))))
(define (& lst1 lst2)
  (cond
    ((null? lst1) (multisubst 0 1 lst2)); 当lst1的长度比lst2短，用0来替换lst2多于lst1的其它元素
    ((null? lst2) (multisubst 0 1 lst1))
    ((and (one? (pick_right lst1))
          (one? (pick_right lst2)))
    (add_1_to_right (& (delete_right lst1) (delete_right lst2)))); 当lst1和lst2最右边的元素皆为1时，使新构建的列表最右边元素为1
    (else (add_0_to_right
           (& (delete_right lst1) (delete_right lst2)))))); 此为当lst1和lst2从右数的第n位同是0或一个1一个0时，使新构建的列表最右边元素为0
 
  
(& '(1 0 1 1 0 0 0 1) '(0 0 1 1 0 1 0 1)) ;#(0 0 1 1 0 0 0 1)
(& '(1 0 1) '(0 1 1 1 1)); (0 0 1 0 1)


;binary to decimal: 将二进制列表转为整数，如(0 0) 就是 0， (0 1) 就是1 (1 1)就是2
;提示：1011对应2^3 + 2^1 + 2^0 = 11 以此类推。 
;利用前面写的位运算来判断某一位比特是否是1

(define (length lst); 获取列表的长度
  (cond
    ((null? lst) 0)
    (else (add1 (length (cdr lst))))))
(length '(1 2 3 4 5))
(define (power_of_2 n); 求取2的n次方
  (cond
   ((zero? n) 1)
   (else (* 2 (power_of_2 (sub1 n))))))
(power_of_2 4)
  

(define (binary_to_decimal lst) 
  (cond
    ((null? lst) 0)
    ((zero? (car lst))
     (+ 0 (binary_to_decimal (cdr lst)))); 当元素为1时，加0到递归中
    (else (one? (car lst))
          (+ (power_of_2 (sub1 (length lst))); 长度是在length的基础上减一
             (binary_to_decimal (cdr lst)))))); 当元素为1时，加(power_of_2 (length lst))到递归中，使用length就可以知道元素处于列表的第几位了

(binary_to_decimal '(1 0 1 1)) 
(binary_to_decimal '(0 0 1 1)) 

;binary addition： 得到一个二进制列表，返回这个二进制列表 + 1的数 如(0 0 1) 返回 (0 1 0), (1 0 1 1) 返回 (1 1 0 0)
(define (subst new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new (cdr lat)))
    (else (cons (car lat) (subst new old (cdr lat))))))
(define (binary_addition lst); 这题的思路是找规律
  (cond
    ((zero? (pick_right lst))
     (add_1_to_right (delete_right lst)))
    (else
     ((one? (pick_right lst))
      (cond
       ((zero? (pick_right (delete_right lst)))
        (add_0_to_right ((subst 1 (pick_right (delete_right lst)) (delete_right lst)))))
       ((one? (pick_right (delete_right lst)))
        (add_0_to_right (binary_addition (delete_right lst)))))))))
    
    
    
 
(binary_addition '(1 0 1 1)) 
;(binary_addition '(0 0 1 1)) # (1 0 0)
;(binary_addition '(1 1 1 1)) # (1 0 0 0 0)