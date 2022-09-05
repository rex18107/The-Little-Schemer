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
;(add_0_to_right '(1 0 1 1))

;version 1

(define (<<_v1 lst n)
  (cond
    ((zero? n) lst)
    (else  (<<_v1 (rember (car lst) (add_0_to_right lst)) (sub1 n)))))   ; 调用了两个函数进行嵌套递归

;version 2
(define (<<_v2 lst n)
  (cond
    ((zero? n) lst);当n为0时，表示位不需要左移
    (else (add_0_to_right (<<_v2 (cdr lst) (sub1 n))))))
; car lst会因为左移而移除，所以直接使用cdr lst作为<<函数的参数列表，那因为递归以及car lst的被移除，n也要进行递减
  
(<<_v1 '(1 0 1 1) 1)
(<<_v2 '(0 0 1 1) 1)
(<<_v2 '(0 0 1 1) 2)

;(<< '(1 0 1 1) 1) #(0 1 1 0)
;(<< '(0 0 1 1) 2) #(1 1 0 0)

;&:  二进制位运算与操作,输入两个列表，返回一个新列表，只有A和B列表中都为1的位才会出现在新列表中显示1否则是0
 

(define (one? n); 判断n是否为1
  (= n 1))

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
; version1
(define (&_v1 lst1 lst2)
  (cond
    ((null? lst1) (multisubst 0 1 lst2)); 当lst1的长度比lst2短,用0来替换lst2多于lst1的其它元素
    ((null? lst2) (multisubst 0 1 lst1))
    ((and (one? (pick_right lst1))
          (one? (pick_right lst2)))
     (add_1_to_right (&_v1 (delete_right lst1) (delete_right lst2)))); 当lst1和lst2最右边的元素皆为1时,使新构建的列表最右边元素为1
    (else (add_0_to_right
           (&_v1 (delete_right lst1) (delete_right lst2)))))); 此为当lst1和lst2从右数的第n位同是0或一个1一个0时，使新构建的列表最右边元素为0

; version2
(define (&_v2 lst1 lst2)
  (cond
    ((and (null? lst1) (null? lst2)) '()); 当lst1和lst2都为空列表时的结束条件
    ((null? lst1) (cons 0 (&_v2 '() (cdr lst2)))); 当lst1为空列表时，使用递归将lst2剩余元素的位都用0代替
    ((null? lst2) (cons 0 (&_v2 '() (cdr lst1)))); 当lst2为空列表时,使用递归将lst1剩余元素的位都用0代替
    (else
     (cond
       ((and (= 1 (pick_right lst1))
             (= 1 (pick_right lst2)))
        (add_1_to_right (&_v2 (delete_right lst1) (delete_right lst2))))
       ; 判断两个列表最右边的元素是否都为1，如是在构建的新列表最右边元素前加1
       (else (add_0_to_right (&_v2 (delete_right lst1) (delete_right lst2))))))))

; version3
; 设想已经将两个二进制列表翻转(先不行进行翻转,设想),(&_ lst1 lst2)函数进行两个二进制列表比较位元素得到一个新列表
(define (&_ lst1 lst2)
  (cond
    ((and (null? lst1) (null? lst2)) '()); 当lst1和lst2都为空列表时的结束条件
    ((null? lst1) (cons 0 (&_ '() (cdr lst2)))); 当lst1为空列表时,使用递归将lst2剩余元素的位都用0代替
    ((null? lst2) (cons 0 (&_ '() (cdr lst1)))); 当lst2为空列表时,使用递归将lst1剩余元素的位都用0代替
    (else
     (cons (cond
             ((and (eq? (car lst1) 1) (eq? (car lst2) 1))1)
             (else 0))
           (&_ (cdr lst1) (cdr lst2))))))

; 先将其中参数列表进行翻转后，调用(&_ lst1 lst2)函数, 得到一个新列表，再将其翻转           
(define (&v2 lst1 lst2)
  (reverse (&_ (reverse lst1) (reverse lst2)))) 
; v1,v2,v3是越来越简化的过程，v3版本设计调用函数很少，减少复杂度，可能是少调用一次函数，就会减少内存，因为不用储存多个地址，也减少时间？（这个不确定）                 
                             
    
(&v2 '(1 0 1 1 0 0 0 1) '(0 0 1 1 0 1 0 1)) ;#(0 0 1 1 0 0 0 1)
(&v2 '(1 0 1) '(0 1 1 1 1)); (0 0 1 0 1)


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
; version 1
(define (binary_to_decimalv1 lst) 
  (cond
    ((null? lst) 0)
    ((zero? (car lst))
     (+ 0 (binary_to_decimalv1 (cdr lst)))); 当元素为1时，加0到递归中
    (else (one? (car lst))
          (+ (power_of_2 (sub1 (length lst))); 长度是在length的基础上减一
             (binary_to_decimalv1 (cdr lst))))))
; 当元素为1时，加(power_of_2 (length lst))到递归中，使用length就可以知道元素处于列表的第几位了

; version 2
; 在这引入参数n来计算函数递归的次数,以确认位的位置
(define (binary_to_decimalv2 lst n)
  (cond
    ((null? lst) 0)
    ((zero? (car (reverse lst)))
     (+ (* 0 (power_of_2 n))
        (binary_to_decimalv2 (delete_right lst) (+ n 1))))
    ; 如果位是0，则在调用剩余元素递归加上0乘2的n次方，并且每调用一次函数递归，n就加一
    (else (= 1 (car (reverse lst)))
          (+ (* 1 (power_of_2 n))
             (binary_to_decimalv2 (delete_right lst) (+ n 1))))))
; 如果位是1,则在调用剩余元素递归加上1乘2的n次方,并且每调用一次函数递归,n就加一
        
            
(binary_to_decimalv2 '(1 0 1 1) 0) 
(binary_to_decimalv2 '(0 0 1 1) 0) 

;binary addition： 得到一个二进制列表，返回这个二进制列表 + 1的数 如(0 0 1) 返回 (0 1 0), (1 0 1 1) 返回 (1 1 0 0)
(define (subst new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new (cdr lat)))
    (else (cons (car lat) (subst new old (cdr lat))))))
(define (all_are_one lst); (all_are_one lst)函数的参数为列表,其判断一个二进制列表里的元素是否皆为1
  (cond
    ((zero? (car lst)) #f)
    ((and (one? (car lst)) (null? (cdr lst))) #t);递归结束条件
    (else (all_are_one (cdr lst)))))
#;(define (binary_addition lst)
    (cond  
      ((zero? (pick_right lst))
       (add_1_to_right (delete_right lst)))
      (else 
       ((one? (pick_right lst))
        (cond
          ((zero? (pick_right (delete_right lst))) 
           (add_0_to_right  (add_1_to_right (delete_right (delete_right lst)))))
          (else ((one? (pick_right (delete_right lst)))
                 (add_0_to_right (binary_addition (delete_right lst))))))))))


; 设想已经将二进制列表翻转（先不行进行翻转，设想），进行二进制加一得到一个列表
(define (binary_add lst)
  (cond
    ;如果列表穷尽了,算法还是没结束,就说明没找到0,全部都是1。我们给列表加个1到尾部(翻转以后是头部)
    ((null? lst) '(1))
    ;如果这一位是0,那么直接改为1,算法结束。
    ((= (car lst) 0) (cons 1 (cdr lst)))
    ;不是0的一律设为0(直到我们撞到0或者列表跑完了)
    (else
     (cons 0 (binary_add (cdr lst))))))
; 先将参数列表翻转，后调用(binary_add lst)函数得到新列表，再将其翻转就得到了答案
(define (binary_addition lst)
  (cond
    ((null? lst) lst)
    (else (reverse (binary_add (reverse lst))))
    ))

 
(binary_addition '(1 0 1 1)) 
;(binary_additionv2 '(0 0 1 1)) 
;(binary_additionv2 '(1 1 1 1))
