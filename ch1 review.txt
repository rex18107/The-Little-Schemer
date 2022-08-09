1.只使用cons建立列表:(4 3 2)
Ans：
(cons 4 (cons 3 (cons 2 null)))

2.取列表(4 3 2)中的（3 2）
Ans：
(cdr '(4 3 2))

3.取列表中的4
Ans：
(car '(4 3 2))

4.你有`(((hotdogs))(and)(pickle)relish))))，使用cdr car取出pickle

Ans：
(let ((l '(((hotdogs))(and)(pickle)relish))) (car (car (cdr (cdr l)))))

5.(null? (cdr (cons 1 null))) 是true还是false？
Ans：
#t
6.(null? (car (cons 1 null))) 是true还是false？
Ans：
#f



