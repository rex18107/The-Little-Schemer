#lang scheme

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; p81
(define (rember* a l); 移除列表l中的所有a元素后构建新列表
  (cond
    ((null? l) '()); 当l为空列表时，返回（）
    ((atom? (car l)); 当l的第一个元素为原子时，有两种情况，（car l）为a及(car l)不为a
     (cond
       ((eq? a (car l))
        (rember* a (cdr l)))
       (else (cons (car l) (rember* a (cdr l))))))
       ; 当(car l)不为a时，要记得将(car l)加到递归函数前面
    (else (cons (rember* a (car l)) (rember* a (cdr l))))))
    ; 此时(car l)是列表，直接将已处理好的函数加到(rember* a (cdr l))，表示进行递归
    
(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))      

; p82
(define (insertR* new old l); 将原子new插入到每一个原子old的后面构建成一个列表
  (cond
    ((null? l) '()); 当l为空列表时,返回()
    ((atom? (car l)); 当(car l)为原子时，有两种情况
     (cond
       ((eq? old (car l));
        (cons old (cons new (insertR* new old (cdr l)))))
       ; (car l)与old相同，返回值为先加上new到(insertR* new old (cdr l))中，再加上(car l)
       (else (cons (car l) (insertR* new old (cdr l))))))
       ; (car l)与old不相同，继续递归除了（car l）外的剩余部分后加上（car l）
    (else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))))
    ; 当（car l）为列表时，先调用insertR函数构建成新列表，将构建好的新（car l）加上至继续递归除了(car l)外的剩余部分
(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood)
          )
