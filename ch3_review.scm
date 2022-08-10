#lang scheme

;完成以下函数


;reverse_two: 接受一个带有2个元素的列表,返回一个新的列表,左边的在右边,右边的在左边


(define (reverse_two l)
  (cond
    ((null? l) '()) ;当列表为空时需要返回空列表
    (else (cons (car (cdr l)) (list (car l))))))
;先获得(cdr l)列表,再取出其第一个元素,也就是l列表第二个元素,接着将(car l)形成列表,然后再cons (car (cdr l))到 (list (car l))上
(reverse_two '(apple (2)));test
    


;(reverse_two '(1 2)) #(2 1)
;(reverse_two '(asd bbb)) #(bbb asd)
;(reverse_two '((1 2) bbb)) #(bbb (1 2))


;build_list: 接受一个数字n,返回从100到 n乘以 100的列表:(100 200 300 .. n * 100)
;提示:使用insertR
(define (insertR new old lat)
  (cond
  ((null? lat) '())
  ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
  (else (cons (car lat) (insertR new old (cdr lat))))))

(define (build_list n)
  (cond
    ((eq? n 1) '(100));如果n为1,代表递归结束，这是个结束条件
    (else (insertR (* n 100) (* (- n 1) 100) (build_list (- n 1))))))
    ;(* n 100)这个元素为函数的开始条件，利用insertR函数将值最大的数值往(build_list （- n 1))的最后一个元素右边插入



;(build_list 10)#(100 200 300 400 500 600 700 800 900 1000)

;reverse: 接受一个带有n个元素的列表,反转里面的元素


#;
(define (reverse lst)
  (cond
    ((null? lst) '());当列表为空时需要返回空列表
    (else (insertR (car lat) (car (cdr lat)) (reverse (cdr lst)))))); (car (cdr lat))表达式会有一个问题，当lst为'（d）情况时，程序报错，因为car的参数不能为空列表
;将列表的第一个元素移到列表第二个元素右边，再引入参数（cdr lst）到reverse函数中。
;递归的重要思想就是先找到第一步的过程，尔后第二、第三乃至第n直接调用递归函数


;(reverse '(1 2 3) #(3 2 1)
;(reverse '(2 1 3) #(3 1 2)
;(reverse '(A C D E) #(E D C A)


;reverse_nested: 接受一个带有n个元素的列表,反转里面的元素,包括里面的子列表中的元素

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define (reverse_nested lst)
  (cond
    ((null? lst) '());当列表为空时需要返回空列表
    ((atom? (car lst)) (insertR (car lst) (car (cdr lst)) (reverse_nested (cdr lst)))); (car (cdr lat))表达式会有一个问题,当lst为'(d)情况时,程序报错,因为car的参数不能为空列表
    (else (insertR (car (car lst)) (car (car (cdr lst))) (reverse_nested (cdr (car lst)))))));(car (car (cdr lst)))跟上注释同理
    
  (reverse_nested '(1 2 3))


;(reverse_nested: '(1 2 3) #(3 2 1)
;(reverse_nested: '(1 2 3 (4 5)) #((5 4) 3 2 1)
;(reverse_nested: '(a (b c d) e) #(e (d c b) a)

#;
(define (reverse_two lst)
(cons (car (cdr lst)) (cons (car lst) null));这里没有((null? lst) '())条件，更简洁
  )

#;
(define (build_list n)
(cond ((eq? n 1) '(100))
      (else (insertR (* n 100) (* (- n 1) 100) (build_list (- n 1))))
  ))

