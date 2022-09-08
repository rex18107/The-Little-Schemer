#lang scheme

;完成以下函数

#|
1.map: 传入lambda匿名函数func 和列表lat，输出lat经过func变换以后输出的值
|#

(define (map func lat)
  (cond
    ((null? lat) '())
    (else
     ; 将func作用在(car lat)上，构筑在(cdr lat)的递归调用函数值上
     (cons
      (func (car lat))
      (map func (cdr lat))))))
; (1 4 9 16 25 36) 
(map (lambda (item) (* item item)) '(1 2 3 4 5 6))
; (0 1 2 3 4 5)
(map (lambda (item) (- item 1)) '(1 2 3 4 5 6)) 
(map (lambda (item) 'even) '(1 2 3 4 5 6)) ;(0 1 2 3 4 5)
(define (test x)
  'haha)
(map test '(1 2 3 4 5 6)) ;(0 1 2 3 4 5)

#|
2.convert: 实现一个lambda匿名函数交给map，使其把lat中的所有数字，如果是偶数就变成字符串even，否则变成odd
|#
; 求商
(define (division n m)
  (cond
    ; 此时n小于m,商为0
    ((< n m) 0)
    (else (+ (division (- n m) m) 1))))
; 判断是否为偶数
(define (even? n)
  (= (* (division n 2) 2) n))
; #f
(even? 1)

; 传入一个参数n到convert中会返回一个以x作为参数的函数
(define convert
  (lambda (n)
    (lambda (x)
      (cond
        ; 如果x为偶数，返回even
        ((n x) 'even)
        ; 否则返回odd
        (else 'odd)))))
                            
;(odd even odd even odd even)         
(map (convert  even?)  '(1 2 3 4 5 6))  

#|
3.convert-factory: 实现一个lambda匿名函数工厂，使其可以制造根据数字是奇数还是偶数，输出不同字符串的函数。 并交给map,使其把lat中的所有数字变换成对应字符串
|#

(define convert-factory
  (lambda (even-word odd-word)
    (lambda (x)
      (cond
        ; x为偶数就输出even-word
        ((even? x) even-word)
        ; x为奇数就输出odd-word
        (else odd-word)))))

; (ODD EVEN ODD EVEN ODD EVEN)
(map (convert-factory 'EVEN 'ODD) '(1 2 3 4 5 6))
; (坏的 坏的 好的 坏的 坏的 好的)
(map (convert-factory '好的 '坏的) '(1 3 1000 1001 5 6)) 


#|
4.both-item 实现一个函数，把2个item放在一个列表里面（给group演示用的一个简单函数）
|#
     
(define (both-item item1 item2)
  (cons item1
        (cons item2 '())))
                    
; ((abc) (def))
(both-item '(abc) '(def))

#|
5.group: 实现一个分组函数，接受三个参数：数组lat，判断函数test? 以及收集器函数col。
其中col是一个可以接受2个参数的函数，这2个参数分别对应test?为true和false的元素列表。（both-item是一个这样的函数）
group会判断所有交给test?以后返回为#t的元素，放入col的第一个参数，而且把#f的元素，放入col的第二个参数
提示：参考multirember&co
|#

    
(define (group lat test? col)
  (cond
    ; 终止条件，将两个空列表作为参数传入进col中
    ((null? lat)
     (col '() '()))
    ; 将test?以后返回为#t的元素,放入col的第一个参数
    ((test? (car lat))
     (group (cdr lat) test? (lambda (nl seen)
                              ; 这里是将新的参数(cons (car lat) nl)和seen
                              ; 传入到上个递归里col的函数内容中
                              (col (cons (car lat) nl)
                                   seen))))
    (else
     ; 将test?以后返回为#t的元素,放入col的第二个参数
     (group (cdr lat) test? (lambda (nl seen)
                              (col nl
                                   (cons (car lat) seen)))))))

; ((2 4 6) (1 3 5))
(group '(1 2 3 4 5 6) even? both-item)
; ((2 4 6 100 700) (1 3 5 221 45 651 701))
(group '(1 2 3 4 5 6 100 221 45 651 700 701) even? both-item)

;6.问题：这个函数应该输出什么，col干了什么？

; col 在这里是对参数列表进行分类，分成偶数列表和奇数列表，
; 再将奇偶列表的第一个元素进行比较，输出第一个元素数值大的列表
; (702 2 4 6 100 700)
(group '(702 2 3 4 5 6 100 221 45 651 700 701) even?
       (lambda (item1 item2)
         (cond
           ((> (car item1) (car  item2)) item1)
           (else item2))))
