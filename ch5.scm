#lang scheme

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; p81
; 移除列表l中的所有a元素后构建新列表
(define (rember* a l)
  (cond
    ; 当l为空列表时,返回()
    ((null? l) '())
    ; 当l的第一个元素为原子时,有两种情况,(car l)为a及(car l)不为a
    ((atom? (car l))
     (cond
       ((eq? a (car l))
        (rember* a (cdr l)))
       (else (cons (car l) (rember* a (cdr l))))))
       ; 当(car l)不为a时,要记得将(car l)加到递归函数前面
       ; 此时(car l)是列表,直接将已处理好的函数加到(rember* a (cdr l)),表示进行递归
       (else (cons (rember* a (car l)) (rember* a (cdr l))))))   
(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))      

; p82
; 将原子new插入到每一个原子old的后面构建成一个列表
(define (insertR* new old l)
  (cond
    ; 当l为空列表时,返回()
    ((null? l) '())
    ; 当(car l)为原子时,有两种情况
    ((atom? (car l))
     (cond
       ((eq? old (car l))
         ; (car l)与old相同,返回值为先加上new到(insertR* new old (cdr l))中,再加上(car l)
        (cons old (cons new (insertR* new old (cdr l)))))
        ; (car l)与old不相同,继续递归除了(car l)外的剩余部分后加上(car l)
       (else (cons (car l) (insertR* new old (cdr l))))))    
    ; 当(car l)为列表时,先调用insertR函数构建成新列表,将构建好的新(car l)加上至继续递归除了(car l)外的剩余部分
    (else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))))
(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))

; p85
;找出列表l中有几个a元素
(define (occur* a l)
  (cond
    ((null? l) 0)
    ; 当列表第一个元素为原子，有两种情况
    ((atom? (car l))
     (+ (cond
       ; a与(car l)相等加1
       ((eq? (car l) a)
       1)
       ;a与(car l)不相等加0
       (else (car l) 0))
       ; 继续递归剩余元素
       (occur* a (cdr l))))
    ; 当列表第一个元素为列表，对其内部调用occur*函数，接着继续递归剩余元素算出有几个a
    (else (+ (occur* a (car l)) (occur* a (cdr l))))))
(occur* 'a '(a (a (a b c)) c d (b (d a)) a)) ; 5

; 将列表中出现的所有old替换成new
(define (subst* new old lat)
  (cond
    ((null? lat) '())
    ; 当列表第一个元素为原子,有两种情况
    ((atom? (car lat))
     (cons (cond
             ; a与(car l)相等直接加new
             ((eq? old (car lat)) new)
             ; a与(car l)不相等加上（car lat)             
             (else (car lat)))
           ; 继续递归剩余元素
           (subst* new old (cdr lat))))
     ; 当列表第一个元素为列表,对其内部调用subst*函数得到一新列表，并加在递归剩余元素所得的列表前
    (else (cons (subst* new old (car lat))
                (subst* new old (cdr lat))))))
(subst* 'h 'a '(a (a (a b c)) c d (b (d a)) a)) ; (h (h (h b c)) c d (b (d h)) h)

; p86
; 在列表中所有出现的old左边插入new
(define (insertL* new old lat)
  (cond
    ((null? lat) '())
    ; 当列表第一个元素为原子,有两种情况
    ((atom? (car lat))
     (cond
       ; a与(car l)相等直接加new加old到调用函数递归前
       ((eq? (car lat) old) (cons new (cons old (insertL* new old (cdr lat)))))
       ; a与(car l)不相等加上(car lat)到调用函数递归前
       (else (cons (car lat) (insertL* new old (cdr lat))))))
    ; 当列表第一个元素为列表,对其内部调用insertL*函数,接着继续递归剩余元素
    (else (cons (insertL* new old (car lat)) (insertL* new old (cdr lat))))))
(insertL* 'h 'a '(a (a (a b c)) c d (b (d a)) a)) ; (h a (h a (h a b c)) c d (b (d h a)) h a)

; p87
;判断a元素是否出现在列表l中
(define (member* a l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
      ((eq? (car l) a) #t)
      (else (member* a(cdr l)))))
    (else
     (or (member* a (car l)) (member* a (cdr l))))))
(member* 'a '((a (a b c)) c d (b (d a)) a))

; p88
; 找出非空S-表达式列表最左边的原子
(define (leftmost l)
  (cond
    ; 因为约定leftmost工作在非空列表上，且该列表不包含空列表元素，所以只需询问两个问题
    ((atom? (car l)) (car l))
    ; 当列表最左边元素是列表时，在（car l）中调用函数递归
    (else (leftmost (car l)))))
(leftmost '((most (cutiest child)) is (xiang)))

; p92
; 判断两个列表是否相等
(define (eqlist?-v1 lst1 lst2)
  (cond
    ; 当两个参数列表都为空是返回#t
    ((and (null? lst1) (null? lst2)) #t)
    ; 当其中一个参数列表为空返回#f，这是排除两个列表有元素个数不对等的情况
    ((or (null? lst1) (null? lst2)) #f)
    ; 当两个列表的第一个元素都为原子时，若相同则调用递归继续比较剩余元素,不同返回#f
    ((and (atom? (car lst1)) (atom? (car lst2)))
     (cond
       ((eq? (car lst1) (car lst2))
        (eqlist?-v1 (cdr lst1) (cdr lst2)))
        (else #f)))
    ; 当两个列表的第一个元素都为原子时,先比较两个列表中第一个元素列表里的第一个元素
    ((and (list? (car lst1)) (list? (car lst2)))
     (cond
       ; 当两个列表的第一个元素都为相同原子时,则调用递归继续比较两个列表中第一个元素列表里的剩余元素
       ((eq? (car (car lst1)) (car (car lst2)))
        (eqlist?-v1 (cdr (car lst1)) (cdr (car lst2))))
       ; 不相同则直接返回#f
       (else #f)))
    (else #f)))
(define (eqlist? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    ((and (atom? (car l1)) (atom? (car l2)))
     (and (eq? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2))))
    ((or (atom? (car l1)) (atom? (car l2))) #f)
    (else
     ; 这里比eqlist?-v1这个版本胜在直接调用递归，简化了步骤，代码更简单
     (and (eqlist? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2))))))
(eqlist? '() '(1 2))

; p92
; 判断两个S-表达式是否相同
(define (equal? s1 s2)
  (cond
    ; 如果两个参数都为原子的话，判断原子是否相同，返回其值即可
    ((and (atom? s1) (atom? s2))
     (eq? s1 s2))
    ; 若其中一个参数为原子，另外一个参数肯定为列表，
    ; 因为上面一行代码已经排除了同时为原子的情况，才进入到这行代码的，所以返回#f
    ((or (atom? s1) (atom? s2)) #f)
    ; 此为两个参数都为列表的情况，则调用eqlist函数比较两参数是否相同
    (else (eqlist? s1 s2))))

; p93
;用equal?来重写eqlist-v3
(define (equalist-v3 l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    ; 这里调用了equal？函数来判断参数可能为原子可能为列表的情况
    (else (equal? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2)))))
(equalist-v3 '((1 2) 3 (4 5 (6))) '((1 2 3 (4 5 (6)))))
; p94
; 移除表达式列表l当中的表达式s（s可为列表or原子）
(define (rember s l)
  (cond
    ((null? l) '())
    ; 这是处理（car l）和s同为原子或同为列表并且相等的情况
    ((equal? (car l) s) (cdr l))
    ; 这里的rember函数不能移除元素为列表里与s相同的表达式
    (else (cons (car l) (rember s (cdr l))))))
(rember '(1 2 3) '(a d (1 2 3)))
(rember '(1 2 3) '(a d (a (1 2 3) b)))
