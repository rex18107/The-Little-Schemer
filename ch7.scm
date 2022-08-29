#lang scheme

; 判断a 是否为列表l的元素
(define (member? a l)
  (cond
   ((null? l) #f)
   ((eq? (car l) a) #t)
   (else (member? a (cdr l)))))
; #t
(member? 'a '(a b a s e c))
; p111
; 判断列表是否有重复的原子出现,没有的话，返回#t
(define (set? l)
  (cond
   ; 列表为空时，表示没有重复的原子出现
   ((null? l) #t)
   ; 每一次递归的第一个元素为剩余元素列表成员时，返回#f
   ((member? (car l) (cdr l)) #f)
   (else  (set? (cdr l)))))
; #f
(set? '(apple 3 pear 4 9 apple 3 4))

; p112
; 将列表中除了第一次出现过的元素以外的其它重复元素剔除
(define (makeset-v1 l)
  (cond
   ((null? l) '())
   ; 如果一个元素在列表后部分有重复出现，则只留下这个元素最后一次出现的情况
   ((member? (car l) (cdr l))
    (makeset-v1 (cdr l)))
   (else (cons (car l)
              (makeset-v1 (cdr l))))))
; (b a d s e c)
(makeset-v1 '(a b a c d s e c))

(define (multirember a l)
  (cond
    ((null? l) '())
    ((eq? (car l) a) (multirember a (cdr l)))
    (else
     (cons (car l)
           (multirember a (cdr l))))))

(define (makeset-v2 l)
  (cond
    ((null? l) '())
    (else
     ; 只留下元素第一次出现的情况，其余重复的都删除
     (cons (car l)
           (makeset-v2 (multirember (car l) (cdr l)))))))
; (a b c d s e)
(makeset-v2 '(a b a c d s e c))
(makeset-v2 '(a b 9 a 3 c 3 d 9 c))

; p114
; 判断s1中的每个原子是否都存在于s2中
(define (subset? s1 s2)
  (cond
    ; s1为空时表示其所有元素皆存在于s2列表中
    ((null? s1) #t)
    ; 判断s1的第一个元素在不在s2中，存在就调用递归判断剩余的s1元素
    ((member? (car s1) s2)
     (subset? (cdr s1) s2))
    (else #f)))
; #t
(subset? '(2 3 d) '(w e d 2 q 3))

(define (subset-v2? s1 s2)
  (cond
    ; s1为空时表示其所有元素皆存在于s2列表中
    ((null? s1) #t)
    ; 判断s1的第一个元素在不在s2中,存在就调用递归判断剩余的s1元素
    (else
     (and(member? (car s1) s2)
         (subset-v2? (cdr s1) s2)))))
; #t
(subset-v2? '(2 3 d) '(w e d 2 q 3))   

; p115
; 判断s1的元素是否跟s2的元素全都一样，但是位置可以不同
(define (eqset? s1 s2)
  (cond
    ; 先调用subset判断s1是否存在于s2中，存在就再调用subset判断s2是否在s1中
    ; ，就能确保s1的元素跟s2的元素全都一样
    ((subset? s1 s2) (subset? s2 s1))
    (else #f)))
; #t
(eqset? '(2 a 1) '(a 1 2))
; 简化eqset?
(define (eqset-v2? s1 s2)
   (and (subset? s1 s2) (subset? s2 s1)))
; #f
(eqset? '(2 a 1 c) '(a 1 2))
