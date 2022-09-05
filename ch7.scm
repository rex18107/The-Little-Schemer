#lang scheme

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
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

; 判断是否有交集，判断s1中至少有一个原子也存在于s2(只属于原子元素，列表元素中包含相同的元素返回#f）
(define (intersect? s1 s2)
  (cond
    ; 表示s1中元素已经比较完，且没有一个原子存在于s2中
    ((null? s1) #f)
    ; 调用member函数判断s1的第一个函数是否存在于s2中，存在返回#t
    ((member? (car s1) s2) #t)
    (else (intersect? (cdr s1) s2))))
; #t
(intersect? '(1 2) '(3 2 4 1))
; #f
(intersect? '(1 2) '(3 (2) 4))

(define (intersect?-v2 s1 s2)
  (cond
    ((null? s1) #f)
    (else
     ; 用or来重写intersect?函数
     (or (member? (car s1) s2)  (intersect?-v2 (cdr s1) s2)))))
; #t
(intersect?-v2 '(1 2) '(3 2 4 1))

; p106
; 求交集
(define (intersect s1 s2)
  (cond
    ; 表示s1中元素已经比较完,且没有一个原子存在于s2中
    ((null? s1) '())
    ; 调用member函数判断s1的第一个函数是否存在于s2中
    ((member? (car s1) s2)
     ; 存在就构筑在调用函数递归所得的新列表中 
     (cons (car s1) (intersect (cdr s1) s2)))
    (else (intersect (cdr s1) s2))))
; (1 2)
(intersect '(1 2) '(3 2 4 1))

; 求并集，将s1中存在于s2中的元素去除，剩余的构筑到s2上成为新列表
(define (union s1 s2)
  (cond
    ((null? s1) s2)
    ; 将s1中存在于s2中的元素去除，避免重复出现在构筑的新列表中
    ((member? (car s1) s2)
     (union (cdr s1) s2))
    (else
     (cons (car s1) (union (cdr s1) s2)))))
; (w a d c e f)
(union '(a c w e f) '(a d c e f))

; p117
; 找出所有集合的交集
(define (intersectall l-set)
  (cond
    ; 如果一个集合中只剩一个列表元素，交集就是唯一的那个元素
    ((null? (cdr l-set)) (car l-set))
    ; 调用函数将所有集合的交集求出
    (else (intersect (car l-set)
                     (intersectall (cdr l-set))))))
; (and cat)
(intersectall '((and cat) (cat dog fish and) (dog monkey cat and)))

; p118
(define (a-pair? x)
  (cond
    ; 排除只有一个元素
    ; 这里感觉有点多余？并不会，如果x为原子的话，((null? (cdr x)) #f)
    ; 会导致程序报错，而有了此行会返回#f
    ((atom? x) #f)
    ((null? x) #f)
    ((null? (cdr x)) #f)
    ((null? (cdr (cdr x))) #t)
    (else #f)))
(a-pair? '3)

; p119
; 以下三个函数主要是用于提升代码的可读性
; pair的第一个元素
(define (first p)
  (car p))
; pair的第二个元素
(define (second p)
  (car (cdr p)))
; 构建第二个元素到空列表中，然后再将第一个元素构筑进来
(define (build s1 s2)
  (cons s1 (cons s2 '())))
; (a d)
(build 'a 'd)
; pair的第三个元素
(define (third p)
  (car (cdr (cdr p))))

; first函数是取出列表l中的每个列表元素里的第一个元素,构成一个新列表
(define (firsts l)
  (cond
    ((null? l) '())
    (else (cons (car (car l)) (firsts (cdr l))))))

; p120
; 判断提取列表所有元素的子列表中第一个元素成一个新列表，判断其是否为集合
(define (fun? rel)
  (set? (firsts rel)))
; #f,因为两个b重复了
(fun? '((d 4) (b 0) (b 9) (e 5)))
; #t
(fun? '((d 4) (c 0) (b 9) (e 5)))

; 实现列表rel中列表元素自身内部翻转
(define (revrel rel)
  (cond
    ((null? rel) '())
    (else
     ; 将翻转后的列表元素构筑到剩余进行递归调用的元素上
     (cons (build
            (second (car rel))
            (first (car rel)))
           (revrel (cdr rel))))))
; ((2 a) (3 c) (4 d))
(revrel '((a 2) (c 3) (d 4)))

; p121
; 设计revpair调转pair内部，如（a d）变成（d a）
(define (revpair pair)
  (build (second pair)
         (first pair)))

; 为了可读性重写revrel函数
(define (revrel-v2 l)
  (cond
    ((null? l) '())
    (else
     (cons
      (revpair (car l))
      (revrel-v2 (cdr l))))))
; ((2 a) (3 c) (4 d))
(revrel-v2 '((a 2) (c 3) (d 4)))

(define (seconds l)
  (cond
    ((null? l) '())
    (else (cons
           (car (cdr (car l)))
           (seconds (cdr l))))))
; (a d a)
(seconds '((1 a) (2 d) (3 a)))
; p122
; 相当于，只能有一个x轴座标对应y轴座标，出现两个不同x对应同一个y就错误
(define (fullfun? fun)
  (set? (seconds fun)))
; #f
(fullfun? '((1 a) (2 d) (3 a)))

; fullfun?的另一种实现方式
(define (one-to-one fun)
  ; 将fun的列表元素内部翻转，再调用fun函数判断
  (fun? (revrel fun)))
; #t
(one-to-one '((1 s) (2 d) (3 a)))
