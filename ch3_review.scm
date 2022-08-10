#lang scheme

;完成以下函数


;reverse_two: 接受一个带有2个元素的列表,返回一个新的列表,左边的在右边,右边的在左边

#;
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
#;
(define (build_list n)
  (cond
    ((zero? n) '(0));如果n为0，就输出一个只含有0元素的列表
    



;(build_list 10)#(100 200 300 400 500 600 700 800 900 1000)

;reverse: 接受一个带有n个元素的列表,反转里面的元素


#;
(define (reverse lst)
(______))



;(reverse '(1 2 3) #(3 2 1)
;(reverse '(2 1 3) #(3 1 2)
;(reverse '(A C D E) #(E D C A)


;reverse_nested: 接受一个带有n个元素的列表,反转里面的元素,包括里面的子列表中的元素

#;
(define (reverse_nested lst)
(______))

;(reverse_nested: '(1 2 3) #(3 2 1)
;(reverse_nested: '(1 2 3 (4 5)) #((5 4) 3 2 1)
;(reverse_nested: '(a (b c d) e) #(e (d c b) a)

#;
(define (reverse_two lst)
(cons (car (cdr lst)) (cons (car lst) null))
  )

#;
(define (build_list n)
(cond ((eq? n 1) '(100))
      (else (insertR (* n 100) (* (- n 1) 100) (build_list (- n 1))))
  ))

