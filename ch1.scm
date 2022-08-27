#lang scheme

; p3
; 判断参数是否为原子（原子是数字，字符）
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; 为什么let中赋值还要再有一个括号? 因为这个括号是代码的偏好设计
; let 表达式的赋值只在表达式内部有效。
(let ((l 'turkey)) (atom? l)); #t
(let ((l '1492)) (atom? l)); #t
(let ((l 'u)) (atom? l)); #t
(let ((l '*abc$)) (atom? l)); #t
(let ((l '(atom))) (atom? l)); #t

; p4
(let ((l '(atom turkey or))) (atom? l)); #f
(let ((l '((atom turkey) or))) (atom? l)); #f

; p5
(let ((l '(a b c))) (car l)); a
(let ((l '((a b c) x y z))) (car l)); (a b c)
;(let ((l 'hotdog)) (car l))
; 不能请求一个原子和空列表的car
; (let ((l '())) (car l))

; p6
(let ((l '(((hotdogs)) (and) (pickle) relish))) (car l)); ((hotdogs))
(let ((l '(((hotdogs)) (and)))) (car (car l))); (hotdogs)
(let ((l '(a b c))) (cdr l)); (b c)
(let ((l '((a b c) x y z))) (car l)); (a b c)
(let ((l '(hamberger))) (car l)); hamberger
(let ((l '((x) t r))) (car l)); (x)
; (let ((l 'hotdog)) (car l))
; 不能请求一个原子和空列表的cdr
; (let ((l '())) (cdr l))

; p7
; 答案是 (((c))),因为任意非空列表的cdr总是另一个列表
(let ((l '((b) (x y) ((c))))) (cdr (cdr l))); (((c)))
; 因为(car l)是个原子,cdr不能以原子为参数
; (let ((l '(a (b (c)) d))) (cdr (car l)))

; p8
(let ((a 'peanut) (l '(butter and jelly))) (cons a l)); (peanut butter and jelly)
(let ((s '(banana and))(l '(peanut butter and jelly))) (cons s l)); ((banana and) peanut butter and jelly)
(let ((a 'peanut) (l '(butter and jelly))) (cons a l)); (peanut butter and jelly)
(let ((s '((help) this))(l '(is very ((hard) to learn)))) (cons s l)); (((help) this) is very ((hard) to learn))
(let ((s '(a b (c)))(l '())) (cons s l)); ((a b (c)))
(let ((s 'a)(l '())) (cons s l)); (a)
; 不会有答案,因为第二个参数l必须是列表
; (let ((s '((a b c)))(l 'b)) (cons s l)) 
; (let ((s 'a)(l 'b)) (cons s l)) 不会有答案

; p9
(let ((s 'a)(l '((b) c d))) (cons s (cdr l))); (a c d)
(null? '()); #t
(let ((l '(a b c))) (null? l)); #f

; p10
; 书本上说没有答案,但运行后结果为#f，实际情况与书籍不同
(let ((a 'spagehetti)) (null? a)); #f
(let ((s 'Harry)) (atom? s)); #t
(let ((s '(Harry had a heap of apples))) (atom? s)); #f

; p11
(let ((l '(Harry had a heap of apples))) (atom? (car l))); #t
(let ((l 'Harry)) (atom? l)); #t
(let ((l '(swing low sweet cherry oat))) (atom? (car (cdr l)))); #t
(let ((l '(swing (low sweet) cherry oat))) (atom? (car (cdr l)))); #f
; eq?判断参数a1，a2是否相同
(let ((a1 'Harry) (a2 'Harry)) (eq? a1 a2)); #t
(let ((a1 'margarine) (a2 'butter)) (eq? a1 a2)); #f

; p12
(let ((l1 '()) (l2 '(strawberry))) (eq? l1 l2)); #f
(let ((n1 6) (n2 7)) (eq? n1 n2)); #f
(let ((l '(Marry had a little lamb chop)) (a 'Mary)) (eq? (car l) a)); #f
(let ((l '(Marry had a little lamb chop)) (a 'Mary)) (eq? (car l) a)); #f
(let ((l '(soured milk)) (a 'milk)) (eq? (cdr l) a)); #f

;p13
(let ((l '(beans beans we need jelly beans))) (eq? (car l) (car (cdr l)))); #t
