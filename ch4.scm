#lang scheme

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; p59
; #t,全部的数字都是原子
(atom? 14)
; #t,但这里不考虑负数
(atom? -3)
; #t,但这里只考虑整数
(atom? 3.2321)
; add1函数是指在参数的值上加一
(define (add1 n)
  ; 给出add1的定义
  (+ n 1))
(add1 '67)

; p59
; sub1函数是指在参数的值上减一
; 给出sub1的定义
(define (sub1 n)
  (- n 1))
(sub1 5)
(sub1 0)

; p60
(zero? 0)
(zero? 1942)
(+ 46 12)
(define (++ n m)
 (cond
   ; 当m为0时,n作为返回结果,要有终止条件
  ((zero? m) n)
  ; 本质上就是每递归一次就在n的基础上加一
  ; cons构建列表,add1构建数字
  (else (add1 (++ n (sub1 m))))))
(++ 46 12)

; p61
(- 14 3)
(- 17 9)
(define (-- n m)
  (cond
    ; 当m=0时，返回n为结束条件
    ((zero? m) n)
    ; 本质上就是每递归一次就在n的基础上减一
    (else (sub1 (-- n (sub1 m))))))
(- 18 25)

; p64
(define (addup tup)
  (cond
    ((null? tup) 0)
    ; addup函数是将tup列表中的所有数字相加
    (else (+ (car tup) (addup (cdr tup))))))
(addup '(12 23 34))

; p65
(define (** n m)
 (cond
   ; 当m为0时,是递归终止条件
  ((zero? m) 0)
  ; 这里的本质上是有m个n相加,每一次递归n就会加一次n,m的个数决定相加几次
  ((eq? 1 m) n)  (else (+ n (** n (sub1 m))))))
(** 2 3)

; p68,tup可为空列表
; 这里的tup+函数是针对于相同长度的列表tup1、tup2,将tup1的第一个数字加到tup2的第一个数字上,后面数字以此类推,形成一个新列表
#; (define (tup+ tup1 tup2)
    (cond
      ; 为了确定tup1和tup2长度一致
     ((and (null? tup1) (null? tup2)) '())
     ; 先将tup1的第一个数字加到tup2的第一个数字上,尔后再用cons将刚所得数字加入递归所得的列表中
     (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))))
;(tup+ '(1 2 3) '(1 2 3))

; p71
; 这是经过修改的tup+函数,这里的两个参数列表长度可以不同
(define (tup+ tup1 tup2)
 (cond
   ; 当tup1的长度小于tup2时,返回tup2列表
  ((null? tup1) tup2)
  ; 当tup2的长度小于tup1时,返回tup1列表
  ((null? tup2) tup1)
  (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))))
(tup+ '(1 2 3 4) '(1 2 3))

; p72
; 比较n是否大于m
(define (> n m)
  (cond
    ((zero? n) #f)
    ; 若此行代码作为第一个条件就会出现(> 5 5)返回值为#t的错误情况
    ((zero? m) #t)
    ; 这里要考虑一个因素m和n都为0时,要返回#f,但是有没有上面这行代码都不会影响,因为已经确保((zero? n) #f)为第一个条件
    ((and (zero? n) (zero? m)) #f)
    (else (> (sub1 n) (sub1 m)))))
(> 5 5)
(> 12 23)

; p73
; 比较n是否小于m
(define (< n m)
  (cond
    ((zero? n) #f) 
    ((zero? m) #t)
    (else (> (sub1 n) (sub1 m)))))

; p74
#;(define (= n m)
  (cond
    ; 先判断(zero? n),若结果为#t,就执行(zero? m)
    ((zero? n) (zero? m))
    ((zero? n) #f)
    (else (= (sub1 n) (sub1 m)))))
; 判断两个参数是否相等
 (define (= n m)
   (cond
     ((> n m) #f)
     ((< n m) #f)
     (else #t)))
; 求n的m次方
(define (power n m)
  (cond
    ; 当m等于0，返回1作为结束条件
    ((zero? m) 1)
    ; 其原理为递归几次,n就乘以几次n
    (else (* n (power n (sub1 m))))))
(power 2 3)

; p75
; 这里答案跟计算的正确值不一样,但是在其它文件测试并无错误
(define (division n m)
  (cond
    ; 此时n小于m,商为0
    ((< n m) 0)
    (else (+ (division (- n m) m) 1))))
; 答案是2
(division 6 3)
; 答案是0
(division 3 9)

; p76
; 求lat列表的元素个数
(define (length lat)
  (cond
    ((null? lat) 0)
    (else (add1 (length (cdr lat))))))
(length '(a b c d e))
; 找寻列表lat的第n个元素
(define (pick n lat)
  (cond
    ; 当n=1时，返回列表第一个元素
    ((zero? (sub1 n)) (car lat))
    (else (pick (sub1 n )  (cdr lat)))))
(pick 3 '(a b c d e))

; p77
; 移除列表lat的第n个元素尔后得出一个列表
(define (rempick n lat)
  (cond
    ((zero? (sub1 n)) (cdr lat))
    ; 因为只是移除列表lat的第n个元素,记得将第n 个元素前的元素都要加上去
    (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))
(rempick 3 '(a b c d e f))
; 移除列表lat里的所有数字元素
(define (no_nums lat)
  (cond
    ((null? lat) '())
    ; 判断元素是否为数字
    ((number? (car lat)) (no_nums (cdr lat)))
    ; 记得把移除的数字元素之前的元素加上
    (else (cons (car lat) (no_nums (cdr lat))))))
(no_nums '(apple 2 3 red sweet 4))

; p78
; 将一个列表lat里的数字元素提取出来,构成一个tup列表
(define (all_nums lat)
  (cond
    ((null? lat) '())
    ; 这是提取出全部数字元素的新列表
    ((number? (car lat)) (cons (car lat) (all_nums (cdr lat))))
    (else (all_nums (cdr lat)))))
(all_nums '(apple 2 3 red sweet 4))
; 判断a1与a2是否相等
(define (eqan? a1 a2)
  (cond
    ; 先判断a1跟a2是否都为数字才能用=判断,否则程序会报错
   ((and (number? a1) (number? a2)) (= a1 a2))
   ; 此处情况是a1跟a2的字符类型不同无法判断所以返回#f
   ((or (number? a1) (number? a2)) #f)
   (else (eq? a1 a2))))
(eqan? 'a 0)
; 判断a1与a2是否相等
#; (define (eqan? a1 a2)
  (cond
    ; 先判断a1跟a2是否都为数字才能用=判断,否则程序会报错
   ((and (number? a1) (number? a2)) (= a1 a2))
   ; 包含了两种情况(1)a1、a2为非数字元素;(2)a1或a2其中一个为数字元素,另一个为非数字元素
   (else (eq? a1 a2))))
;上列程序是eqan？函数的化简
; 统计a元素在列表lat当中出现的次数
(define (occur a lat)
  (cond
    ((null? lat) 0)
    ; 当a元素出现一次就加一计数
    ((eq? a (car lat)) (add1 (occur a (cdr lat))))
    (else (occur a (cdr lat)))))
(occur 'c '(a b c d c e c))

; p79
; 判断n是否为1
(define (one? n) 
  (cond
    ((zero? n) #f)
    (else (zero? (sub1 n)))))
(one? 3)
; 判断n是否为1
#;
(define (one? n) 
  (cond
    (else (zero? (sub1 n)))))
; 这是one函数的简化 
#;
(define (one? n)
  (= n 1))
(one? 3)
; 移除列表lat的第n个元素尔后得出一个列表,这是rempick的另一部分写法
(define (rempicks n lat)
  (cond
    ((one? n) (cdr lat))
    ; 因为只是移除列表lat的第n个元素,记得将第n 个元素前的元素都要加上去
    (else (cons (car lat) (rempicks (sub1 n) (cdr lat))))))
(rempicks '3 '(a c d w d))
