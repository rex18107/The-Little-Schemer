#lang scheme

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; p176
; 寻找name在entry列表里对应的元素
(define (lookup-in-entry
         name entry entry-f)
  ; 调用lookup-in-entry-help函数
  (lookup-in-entry-help
   name (first entry)
   (second entry) entry-f))

; 一个辅助函数
(define (lookup-in-entry-help
         name names values
         entry-f)
  (cond
    ; 如果names是空列表，返回以name为参数的值
    ((null? names) (entry-f name))
    ; name等于names的第一个元素，返回values列表的第一个元素
    ((eq? (car names) name)
     (car values))
    ; 以上条件都不满足就继续递归
    (else (lookup-in-entry-help name
                                (cdr names)
                                (cdr values)
                                entry-f))))

; p177
; table为(((a b c)
;          (d e f))
;         ((beverage dessert)
;          ((a b) (c d e))))
; 这样的形式
(define (lookup-in-table
         name table table-f)
  (cond
    ((null? table) (table-f name))
    (else
     (lookup-in-entry name
                      (car table)
                      (lambda (name)
                        (lookup-in-table name
                                    (cdr table)))))))

; 求n的m次幂
(define (^ n m)
  (cond
    ((zero? m) 1)
    (else (* n (* n (- m 1))))))
     
; p105
; 一个算术表达式的表示方式中的第一个子表达式
(define (1st-sub-exp aexp)
  (car (cdr aexp)))

; p106
; 一个算术表达式的表示方式中的第二个子表达式
(define (2nd-sub-exp aexp)
  (car (cdr (cdr aexp))))

; 表达操作符所在位置
(define (operator aexp)
  (car aexp))

; 求出算术表达式
(define (value nexp)
  (cond
    ; 先判断表达式是不是原子,是的话返回原子作为算术表达式
    ((atom? nexp) nexp)
    ; 列表只有一个元素时，返回整个列表
    ((eq? (cdr nexp) null)
     nexp)
    ; 判断列表第二个元素是否为+,是的话返回第一个元素和第三个元素相加的值
    ((eq? (car (cdr nexp)) '+)
     (+ (value (car nexp))
        (value (car (cdr (cdr nexp))))))
    ; 判断列表第二个元素是否为*,是的话返回第一个元素和第三个元素相乘的值
    ((eq? (car (cdr nexp)) '*)
     (* (value (car nexp))
        (value (car (cdr (cdr nexp))))))
    ; 返回第一个元素的第三个元素值的次方
    (^ (value (car nexp))
        (value (car (cdr (cdr nexp)))))))

; add1函数是指在参数的值上加一
(define (add1 n)
  ; 给出add1的定义
  (+ n 1))

; p179
; 7
(value (add1 6))

; 6
(value 6)

; (nothing)
(value '(nothing))

; nothing
(value 'nothing)

; ((from nothing comes somthing))
(value ((lambda (nothing)
          (cons nothing
                '()))
        '(from nothing comes somthing)))

; somthing
(value ((lambda (nothing)
          (cond
            (nothing 'somthing)
            (else 'nothing)))
        #t))

; p180
; #f
(value #f)

; p181
; 针对每种S-表达式产生正确的动作（或函数）
(define (expression-to-action e)
  (cond
    ; 参数为原子，返回此函数
    ((atom? e) (atom-to-action e))
    (else
     ; 参数为列表返回此函数
     (list-to-action e))))

(define (atom-to-action e)
  (cond
    ((number? e) *const)
    ((eq? e #f) *const)
    ((eq? e #f) *const)
    ((eq? e 'cons) *const)
    ((eq? e 'car) *const)
    ((eq? e 'cdr) *const)
    ((eq? e 'null?) *const)
    ((eq? e 'eq?) *const)
    ((eq? e 'atom?) *const)
    ((eq? e 'zero?) *const)
    ((eq? e 'add1) *const)
    ((eq? e 'sub1) *const)
    ((eq? e 'number?) *const)
    (else *identifier)))

; p182
; expression-to-action的辅助函数
(define (list-to-action e)
  (cond
    ((atom? (car e))
     (cond
       ((eq? (car e) 'quote)
        *quote)
       ((eq? (car e) 'lambda)
        *lambda)
       ((eq? (car e) 'cond)
        *cond)
       (else *application))
     (else *application))))

(define (value e)
  ; '()是一个空table
  (meaning e '()))

(define (meaning e table)
  ((expression-to-action e) e table))

; p183
; 针对常量的动作
(define (*const e table)
  (cond
    ((number? e) e)
    ((eq? e #t) #t)
    ((eq? e #f) #f)
    (else
     ; 其它常量类型的原子都表述为primitive
     (build 'premitive e))))

(define (*quote e table)
  (text-of e))

; 这里table包含identifier的值
(define (*identifier e table)
  (lookup-in-table
   e table initial-table))

(define (initial-table name)
  (car '()))

; p184
(define (*lambda e table)
  (build 'non-primitive
         (cons table (cdr e))))

; p185
(define (evcon lines table)
  (cond
    ((else? (question-of (car lines)))
     (meaning (answer-of (car lines))
              table))
    ((meaning (question-of (car lines))
              table)
     (meaning (answer-of (car lines))
              table))
    (else (evcon (cdv lines) table))))

(define (else? x)
  (cond
    ((atom? x) (eq? x 'else))
    (else #f)))

(define (*cond e table)
  (evcon (cond-lines-of e) table))
