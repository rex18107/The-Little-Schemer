#lang scheme

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; add1函数是指在参数的值上加一
(define (add1 n)
  (+ n 1))
; sub1函数是指在参数的值上减一
; 给出sub1的定义
(define (sub1 n)
  (- n 1))
; 判断参数是否为一
(define (one? n)
  (zero? (sub1 n)))
; pair的第一个元素
(define (first p)
  (car p))
; pair的第二个元素
(define (second p)
  (car (cdr p)))
; 构建第二个元素到空列表中,然后再将第一个元素构筑进来
(define (build s1 s2)
  (cons s1 (cons s2 '())))
; 设计revpair调转pair内部,如(a d)变成(d a)
(define (revpair pair)
  (build (second pair)
         (first pair)))
; 找出列表lat的第n个元素
(define (pick n lat)
  (cond
    ; 当n=1时,返回列表第一个元素
    ((zero? (sub1 n)) (car lat))
    (else (pick (sub1 n )  (cdr lat)))))
; 判断是否为列表
(define (a-pair? x)
  (cond
    ; 排除只有一个元素
    ; 这里感觉有点多余?并不会,如果x为原子的话,((null? (cdr x)) #f)
    ; 会导致程序报错,而有了此行会返回#f
    ((atom? x) #f)
    ((null? x) #f)
    ((null? (cdr x)) #f)
    ((null? (cdr (cdr x))) #t)
    (else #f)))
; 求商
(define (division n m)
  (cond
    ; 此时n小于m,商为0
    ((< n m) 0)
    (else (+ (division (- n m) m) 1))))
; 判断是否为偶数
(define (even? n)
  (= (* (division n 2) 2) n))

; p149
(define (looking a lat)
  (keep-looking a (pick 1 lat) lat))

; p150
; 
(define (keep-looking a sorn lat)
  (cond
    ; 如果sorn为数字，调用自己
    ((number? sorn)
     (keep-looking a (pick sorn lat) lat))
    (else
     (eq? sorn a))))
(looking 'cat '(2 4 10 cat 5))

; p151
; 这是一个非一般性的递归函数，是个部分函数
(define (eternity x)
  (eternity x))

; p152
; 参数pair的第一个构成也是一个pair。
; 该函数通过将第一个构成的第二部分移进第二个构成，
; 来构建出一个新的pair
; eg：x是((a b) (c d)), (shift x)的值为
; (a (b (c d)))
(define (shift pair)
  (build (first (first pair))
         (build (second (first pair))
                (second pair))))
; p152
(define (align pora)
  (cond
    ((atom? pora) pora)
    ((a-pair? (first pora))
     (align (shift pora)))
    (else (build (first pora)
                 (align (second pora))))))
; (a (b (c d)))
(align '((a b) (c d)))
; (a (c d))
(align '(a (c d)))
; p154
(define (weight* pora)
  (cond
    ((atom? pora) 1)
    (else
     (+ (* (weight* (first pora)) 2)
        (weight* (second pora))))))
; 9
(weight* '((a b) (c d)))
; 7
(weight* '((a b) c))
         
; shuffle不是全函数
(define (shuffle pora)
  (cond
    ((atom? pora) pora)
    ; 当pora是((a b) (c d))时
    ((a-pair? (first pora))
     ; 这行代码会反复轮流输出((a b) (c d))和
     ; ((c d) (a b))两值，形成循环
     (shuffle (revpair pora)))
    (else
     (build (first pora)
            (shuffle (second pora))))))
; (a (b c))
(shuffle '(a (b c)))
; 没有答案
(shuffle '((a b) (c d)))

; p155
; C不对0生成值，除此之外不知道其是否为全函数
(define (C n)
  (cond
    ((one? n) 1)
    (else
     (cond
       ((even? n)
        (C (division n 2)))
       (else
        (C (add1 (* 3 n))))))))

;p156
; A是全函数
(define (A n m)
  (cond
    ((zero? n) (add1 m))
    ((zero? m) (A (sub1 n) 1))
    (else
     (A (sub1 n)
        (A n (sub1 m))))))
; 没有答案
(A 4 3)

; p160
; length0,判断空列表的长度
(let ((test?
       (lambda (l)
         (cond
           ((null? l) 0)
           (else
            (add1 (eternity (cdr l))))))))
  ; 为什么这没有返回0呢?
  (test? '()))

; p161
; length1,判断包含一个及以下的列表长度的函数
(lambda (l)
  (cond
    ((null? l) 0)
    (else
     (add1
      ((lambda (l)
         (cond
           ((null? l) 0)
           (else
            (add1 (eternity (cdr l))))))
       (cdr l))))))

; p162
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else
        (add1 (length (cdr l)))))))
 eternity)

; p164
((lambda (mk-length)
   (mk-length mk-length))
   (lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l))))))))

; p166
((lambda (mk-length)
   (mk-length mk-length)
   (lambda (mk-length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1
                ((mk-length eternity)
                 (cdr l)))))))))

; p168
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    (mk-length mk-length))))

; 无结果?
(let ((test ((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    (mk-length mk-length))))))
  (test '(a c)))

#|
TLS-ch9 相关心得总结皆在博客中，此file主要记录代码
博客网址：https://poling87109.blogspot.com/2022/09/tls-ch9.html
|#
