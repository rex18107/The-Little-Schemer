#lang scheme

;完成以下函数

#|
1.rotate: 给你一个列表,将列表中的元素向右轮转 k 个位置,其中 k 是非负数。
输入: nums = (1 2 3 4 5 6 7), k = 3
输出: (5 6 7 1 2 3 4)
k = 3意味着所有元素都会往右边跑3格，对于已经在最右边的7，会跑到左边去，所以最右边的7跑到最左边第三格去了。 

输入：nums = (-1, -100 3 99), k = 2
输出：(3 99 -1 -100)
解释: 
向右轮转 1 步: (99 -1 -100 3)
向右轮转 2 步: (3 99 -1 -100)

提示：使用字符串反转实现，比如说(1 2 3 4 5 6 7), k = 3 第一轮全部翻转，得到7654321
第二轮翻转[0,2]的部分，得到5674321
第三轮翻转[3,6]的部分，得到5671234
|#

; version1
; 取最右边的元素
(define (pick_right lst)
  (cond
    ; 其结束条件为此列表递归至只剩一个元素在列表中
    ((null? (cdr lst)) (car lst))
    (else (pick_right (cdr lst)))))
; 删除最右边的元素后返回列表
(define (delete_right lst)
  (cond
    ; 此结束条件做到了删除最右边的元素
    ((null? (cdr lst)) '())
    (else (cons (car lst) (delete_right (cdr lst))))))
(define (rotate-v1 lst k)
  (cond
    ; 列表为空时，返回空列表，此为结束条件
    ((null? lst) '())
    ; k为0时返回原始列表
    ((zero? k) lst)
    ; 将列表最右边元素加到删掉最右边元素的列表中形成一个新列表，并以其作为参数调用rotate函数递归
    (else (rotate-v1 (cons (pick_right lst) (delete_right lst)) (- k 1)))))

(rotate-v1 '(1 2 3 4 5 6 7) 3) ;(5 6 7 1 2 3 4)
(rotate-v1 '(0 0 1 1) 1);(1 0 0 1)

#|
2. subsets: 给你一个整数列表 nums ,列表中的元素 互不相同 。返回该列表所有可能的子集(幂集)。
解集 不能 包含重复的子集。你可以按 任意顺序 返回解集。

输入:nums = (1 2 3)
输出:(() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3))

注意观察：我们有1 2 3 但是没有 3 2 1，因为这不是个排列问题。 我们可以返回 (3 2 1)，但是那样就不能再返回(1 2 3) 因为我们不返回重复的子集。
提示：可以使用之前的二进制递增来判断子集，比如说1代表拿,0代表不拿
那么对于(1 2 3) 我们第一轮是(0 0 0)，产生一个()空集
第二轮是(0 0 1),1代表我们取第三位,产生(3)
第三轮是(0 1 0),1代表我们取第二位,产生(2)
第三轮是(0 1 1),1代表我们取第二位和第三位,产生(2 3) 以此类推就可以解决问题。
|#
; 求lat列表的元素个数
(define (length lat)
  (cond
    ((null? lat) 0)
    (else (add1 (length (cdr lat))))))
; 给出列表长度,得到对应的全是k元素的列表,如给n=2,k=1 ,得到(1 1)
(define (make-0&1-list n k)
    (cond
      ((zero? n) '()) 
      (else (cons k (make-0&1-list (- n 1) k))))) 
(make-0&1-list 2 0)
(make-0&1-list 4 1)
; 二进制加1后所得的列表
(define (binary-add lst)
  (cond
    ;如果列表穷尽了,算法还是没结束,就说明没找到0,全部都是1。我们给列表加个1到尾部(翻转以后是头部)
    ((null? lst) '(1))
    ;如果这一位是0,那么直接改为1,算法结束。
    ((= (car lst) 0) (cons 1 (cdr lst)))
    ;不是0的一律设为0(直到我们撞到0或者列表跑完了)
    (else
     (cons 0 (binary-add (cdr lst))))))
; 给出列表(0 0 ... 0),得到从(0 0 ... 0)加到(1 1 ... 1)的每个二进制列表
(define (get-subsets lst)
  (cond
    ; 如果lst加1后的元素长度大于其原本的长度就返回空列表
    ((> (length (binary-add lst))
        (length lst))
     (cons lst '()))
    (else (cons lst (get-subsets (binary-add lst))))))
(get-subsets '(0 0 0 0))
; 提取二进制列表lst1中的1元素对应lst2位置的元素，构成新列表
(define (corresponding-element lst1 lst2)
  (cond
    ((null? lst1) '())
    ((= (car lst1) 1)
     (cons (car lst2) (corresponding-element (cdr lst1) (cdr lst2))))
    (else (corresponding-element (cdr lst1) (cdr lst2)))))
; lst列表中的元素为二进制列表，将这个列表中的所有元素都用corresponding-element转换，构成新列表
(define (switch-to-subsets lst lsto)
  (cond
    ; 当列表只剩一个元素时，直接转换此列表加在空列表中
    ((null? (cdr lst)) (cons (corresponding-element (car lst) lsto) '()))
    (else (cons
           (corresponding-element (car lst) lsto)
           (switch-to-subsets (cdr lst) lsto)))))
(switch-to-subsets '((0 1 0 1)) '(2 3 4 5))
; 给出一个整数列表的所有子集
(define (subsets lst)
  (switch-to-subsets (get-subsets (make-0&1-list (length lst) 0)) lst))
  
    
       
(subsets '(1 2 3)) ;(() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3))

#|
3.count_islands: 给你一个由 '1'(陆地)和 '0'(水)组成的的二维网格,请你计算网格中岛屿的数量。
岛屿总是被水包围,并且每座岛屿只能由水平方向和/或竖直方向上相邻的陆地连接形成（也就是说只有上下左右联通的才算是一个岛屿，斜线的不算）
此外,你可以假设该网格的四条边均被水包围。

输入:grid = (
  (1 1 1 1 0) 
  (1 1 0 1 0) 
  (1 1 0 0 0) 
  (0 0 0 0 0)
)
输出:1 (只有一个岛屿)

输入:grid = (
  (1 1 0 0 0) 
  (1 1 0 0 0) 
  (0 0 1 0 0) 
  (0 0 0 1 1)
)
输出:3(左上角一个四个1的岛屿,第三行有个只有一个1的岛屿,右下角有个有两个1的岛屿,一共三个

提示：这道题用深度优先搜索就可以，但是怎么实现呢？
这道题欢迎网上查题解找思路，重点在于实现。
https://leetcode.cn/problems/number-of-islands/solution/dao-yu-shu-liang-by-leetcode/
|#

#;
(define (count_islands island)
  
  )

;(count_islands '((1 1 1 1 0) (1 1 0 1 0) (1 1 0 0 0) (0 0 0 0 1));2
