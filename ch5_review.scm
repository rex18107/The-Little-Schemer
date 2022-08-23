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

(define (binary-add lst)
  (cond
    ;如果列表穷尽了,算法还是没结束,就说明没找到0,全部都是1。我们给列表加个1到尾部(翻转以后是头部)
    ((null? lst) '(1))
    ;如果这一位是0,那么直接改为1,算法结束。
    ((= (car lst) 0) (cons 1 (cdr lst)))
    ;不是0的一律设为0(直到我们撞到0或者列表跑完了)
    (else
     (cons 0 (binary-add (cdr lst))))))
; 求lat列表的元素个数
(define (length lat)
  (cond
    ((null? lat) 0)
    (else (add1 (length (cdr lat))))))
; 假设给出长度3,求得到((0 0 0)(1 1 1)),只是这里的长度为k
(define (all-0&1 k0 k1 n lst0 lst1) 
  (cond
    ; 表示此时已经列完有k个元素的(0 0 ... 0)和(1 1 ... 1)
    ((or (zero? k0) (zero? k1))'())
    ; 列完有k个元素的(0 0 ... 0),(1 1 ... 1)将其整合为((0 0 ... 0) (1 1 ... 1))输出
    ((and (= (length lst0) n)
          (= (length lst1) n))
     (cons lst0 (cons lst1 '())))
    ; 只要k0还大于0,持续递归调用将0加至lst0
    ((> k0 0) (cons 0 (all-0&1 (- k0 1) k1 n lst0 lst1)))
    ; 只要k1还大于1,持续递归调用将1加至lst1
    (else (> k1 0) (cons 1 (all-0&1  k0 (- k1 1) n lst0 lst1)))))
; 输出列表中有几个a元素
(define (count lst a)
  (cond
    ((null? lst) 0)
    ((= (car lst) a) (+ (count(cdr lst)) 1))
    (else (count (cdr lst) a))))
; 给出列表形式为k个0元素的列表,将其所有子集列出来,lst的形式为(0 0 ... 0),lsts为'()
(define (list-binaryadd lst lsts k)
  (cond
    ((zero? k) lsts)
    ((> k 0) (cons (binary-add lst)
                   (list-binaryadd (binary-add lst) lsts (- k 1))))
; 找出二进制列表里为1位置所对应的另一个列表元素
(define (corresponding-element lst1 lst2)
  (cond
    ((null? lst1) '())
    ((= (car lst1) 1) (cons (car lst2) (corresponding-element (cdr lst1) (cdr lst2))))
    (else (corresponding-element (cdr lst1) (cdr lst2)))))

; k作为lst的长度
(define (subsets lst k)
  (cond            
    ((zero? (- k 1)) lst)))
  
    
       
;(subsets '(1 2 3) ;(() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3))

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
