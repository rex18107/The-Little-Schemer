#lang scheme

; 完成以下函数
; reverse_two: 接受一个带有2个元素的列表,返回一个新的列表,左边的在右边,右边的在左边
(define (reverse_two l)
  (cond
    ; 当列表为空时需要返回空列表
    ((null? l) '())
    ; 先获得(cdr l)列表,再取出其第一个元素,也就是l列表第二个元素,接着将(car l)形成列表,然后再cons (car (cdr l))到 (list (car l))上
    (else (cons (car (cdr l)) (list (car l))))))
(reverse_two '(apple (2)));test
; (reverse_two '(1 2)) #(2 1)
; (reverse_two '(asd bbb)) #(bbb asd)
; (reverse_two '((1 2) bbb)) #(bbb (1 2))


; build_list: 接受一个数字n,返回从100到 n乘以 100的列表:(100 200 300 .. n * 100)
; 提示:使用insertR
(define (insertR new old lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
    (else (cons (car lat) (insertR new old (cdr lat))))))

(define (build_list n)
  (cond
    ; 如果n为1,代表递归结束,这是个结束条件
    ((eq? n 1) '(100))
     ; (* n 100)这个元素为函数的开始条件,利用insertR函数将最大的数值(* n 100)往(build_list (- n 1))的最后一个元素右边插入
    (else (insertR (* n 100) (* (- n 1) 100) (build_list (- n 1))))))
; (build_list 10)#(100 200 300 400 500 600 700 800 900 1000)
; reverse: 接受一个带有n个元素的列表,反转里面的元素

;version 1
#;
(define (reverse lst)
  (cond
    ; 这是reverse函数的终止条件,可以解决下一行注释问题
    ((null? (cdr lst))  lst)
    ; (car (cdr lat))表达式会有一个问题,当lst为'(d)情况时,程序报错,因为car的参数不能为空列表
    (else (insertR (car lst) (car (cdr lst)) (reverse (cdr lst))))))
; 将列表的第一个元素移到列表第二个元素右边,再引入参数(cdr lst)到reverse函数中。
; 递归的重要思想就是先找到第一步的过程,尔后第二、第三乃至第n步直接调用递归函数


; version 2
; 取最右边的元素
(define (pick_right lst)
  (cond
    ; 其结束条件为此列表递归至只剩一个元素在列表中
    ((null? (cdr lst)) (car lst))
    (else (pick_right (cdr lst)))))
; 删除最右边的元素后返回列表
(define (delete_right lst)
  (cond
    ; 此结束条件做到了删除最右边的元素
    ((null? (cdr lst)) '())
    (else (cons (car lst) (delete_right (cdr lst))))))
(delete_right '(1 2 3))
(define (reverse lst)
  (cond
    ((null? (cdr lst)) lst) 
    (else (cons (pick_right lst) (reverse (delete_right lst))))))
; 上面代码的思想是将列表的最后一个程序移到列表的第一个元素之前，并且删掉最后一个元素，这里
; 用到两个辅助函数delete_right和pick_right
(reverse '(b 2 3 a 5))
; (reverse '(1 2 3) #(3 2 1)
; (reverse '(2 1 3) #(3 1 2)
; (reverse '(A C D E) #(E D C A)


; reverse_nested: 接受一个带有n个元素的列表,反转里面的元素,包括里面的子列表中的元素
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


;翻转列表(包括列表中的嵌套子列表)
; 奈修改版本
(define (reverse_nested lst)
  (cond
     ; 关于原子条件的询问,按照我原来的思维认为只能是针对于列表中的最右一个元素进行询问,
    ;所以不能理解为什么语句((atom? (pick_right lst)) lst)不正确,因为这里忽略了一个问题,lst是可以为原子的
    ((atom? lst) lst)
    ((null? lst) lst)
    (else (cons (reverse_nested (pick_right lst)) (reverse_nested (delete_right lst))))))
; 未修改前的错误函数
#;
(define (reverse_nested lst) 
  (cond
    ; 当lst最后一个元素为列表时,先进行列表元素的翻转
     ((list? (pick_right lst)) (cons  (reverse_nested (pick_right lst))  (reverse_nested (delete_right lst))))      
     ((null? (cdr lst)) 
       (cond 
        ((atom? (pick_right lst)) lst)
        ; 结束条件的(car lst)有原子和列表两种可能性
        (else (reverse_nested lst))))
     ; 此时是pick_right为原子的情况
    (else (cons (pick_right lst) (reverse_nested (delete_right lst))))))
(reverse_nested '(1 2 3 (A B C (B D (A B) A)) A A D))
     ; 原本针对于reverse_nested函数，我的设计的reverse_nested_wrong会有两个cons跟两个cond，
     ; 因为考虑到列表的最右边元素可能为原子或列表，但是奈修改的版本
     ; ((atom? lst) lst)和((null? lst) lst)可以解决这个问题，使得代码优化，
     ; 也不会出现遇到lst为（（AB））时，未修改reverse_nested报错的情况，因为lst为原子的话，（cdr lst）不成立
(reverse_nested '(1 2 3)) ;(3 2 1)
(reverse_nested '(1 2 3 (4 5))) ;((5 4) 3 2 1)
(reverse_nested '(a (b c d) e)) ;(e (d c b) a)
(reverse_nested '((A C)))
(reverse_nested '((A C (A B))))
(reverse_nested '((A D D (A B 1 2 3))))

 ;(reverse_nested: '(1 2 3) #(3 2 1)
;(reverse_nested: '(1 2 3 (4 5)) #((5 4) 3 2 1)
;(reverse_nested: '(a (b c d) e) #(e (d c b) a)

#;
(define (reverse_two lst)
  ;这里没有((null? lst) '())条件,更简洁
  (cons (car (cdr lst)) (cons (car lst) null))
  )

#;
(define (build_list n)
  (cond ((eq? n 1) '(100))
      (else (insertR (* n 100) (* (- n 1) 100) (build_list (- n 1))))
  ))
