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
(define (makeset l)
  (cond
   ((null? l) '())
   ; 如果一个元素在列表后部分有重复出现，则只留下这个元素最后一次出现的情况
   ((member? (car l) (cdr l))
    (makeset (cdr l)))
   (else (cons (car l)
              (makeset (cdr l))))))
; (b a d s e c)
(makeset '(a b a c d s e c))
