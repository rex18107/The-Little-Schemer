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
     