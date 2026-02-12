# Chez Scheme 练习题

这些练习题旨在帮助你掌握 Chez Scheme 的核心概念和高级特性。每个练习都包含问题描述、提示和参考答案。

## 基础练习

### 练习 1: 列表操作

**问题**: 实现一个函数 `flatten`，将嵌套列表展平为一维列表。

```scheme
;; 示例
(flatten '(1 (2 3) (4 (5 6))))  ; => (1 2 3 4 5 6)
(flatten '((1) ((2)) (((3)))))  ; => (1 2 3)
```

**提示**: 使用递归，检查元素是否为列表。

<details>
<summary>查看答案</summary>

```scheme
(define (flatten lst)
  (cond
    [(null? lst) '()]
    [(pair? (car lst))
     (append (flatten (car lst)) (flatten (cdr lst)))]
    [else
     (cons (car lst) (flatten (cdr lst)))]))

;; 尾递归版本
(define (flatten-tail lst)
  (let loop ([lst lst] [acc '()])
    (cond
      [(null? lst) (reverse acc)]
      [(pair? (car lst))
       (loop (cdr lst) (loop (car lst) acc))]
      [else
       (loop (cdr lst) (cons (car lst) acc))])))
```
</details>

---

### 练习 2: 高阶函数

**问题**: 实现 `compose` 和 `curry` 函数。

```scheme
;; compose: (compose f g) 返回 f ∘ g
;; curry: 将二元函数转换为柯里化形式
```

**提示**: `compose` 返回一个新函数，`curry` 返回嵌套的 lambda。

<details>
<summary>查看答案</summary>

```scheme
;; compose
(define (compose f g)
  (lambda (x) (f (g x))))

;; 多函数组合
(define (compose* . funcs)
  (if (null? funcs)
      (lambda (x) x)
      (fold-right compose (lambda (x) x) funcs)))

;; curry
(define (curry f)
  (lambda (x)
    (lambda (y)
      (f x y))))

;; 使用示例
(define add1 (lambda (x) (+ x 1)))
(define double (lambda (x) (* x 2)))
(define f (compose double add1))
(f 5)  ; => 12

(define curried-add (curry +))
((curried-add 3) 4)  ; => 7
```
</details>

---

### 练习 3: 快速排序

**问题**: 实现快速排序算法。

**提示**: 选择枢轴，分区，递归排序。

<details>
<summary>查看答案</summary>

```scheme
(define (quicksort lst)
  (if (null? lst)
      '()
      (let ([pivot (car lst)]
            [rest (cdr lst)])
        (let-values ([(smaller larger)
                      (partition (lambda (x) (< x pivot)) rest)])
          (append (quicksort smaller)
                  (list pivot)
                  (quicksort larger))))))

;; partition 辅助函数
(define (partition pred lst)
  (let loop ([lst lst] [true-list '()] [false-list '()])
    (cond
      [(null? lst) (values (reverse true-list) (reverse false-list))]
      [(pred (car lst))
       (loop (cdr lst) (cons (car lst) true-list) false-list)]
      [else
       (loop (cdr lst) true-list (cons (car lst) false-list))])))

;; 测试
(quicksort '(3 7 1 9 2 8 4 6 5))  ; => (1 2 3 4 5 6 7 8 9)
```
</details>

---

## 宏练习

### 练习 4: 循环宏

**问题**: 实现一个 `while` 宏。

```scheme
(while condition
  body ...)
```

**提示**: 使用 `syntax-rules`，转换为 `let` 循环。

<details>
<summary>查看答案</summary>

```scheme
(define-syntax while
  (syntax-rules ()
    [(_ condition body ...)
     (let loop ()
       (when condition
         body ...
         (loop)))]))

;; 使用示例
(let ([i 0])
  (while (< i 5)
    (display i)
    (display " ")
    (set! i (+ i 1))))
;; 输出: 0 1 2 3 4
```
</details>

---

### 练习 5: 模式匹配宏

**问题**: 实现一个简化的 `match` 宏，支持常量和变量模式。

```scheme
(match value
  [0 'zero]
  [1 'one]
  [x x])
```

**提示**: 使用 `syntax-case`。

<details>
<summary>查看答案</summary>

```scheme
(define-syntax match
  (syntax-rules ()
    [(_ val) (error 'match "no matching clause")]
    [(_ val [pattern body] rest ...)
     (let ([tmp val])
       (if (equal? tmp 'pattern)
           body
           (match tmp rest ...)))]
    [(_ val [var body] rest ...)
     (let ([var val])
       body)]))

;; 使用示例
(match 5
  [0 'zero]
  [1 'one]
  [x (list 'other x)])  ; => (other 5)
```
</details>

---

## 记录类型练习

### 练习 6: 二叉树

**问题**: 使用记录类型实现二叉树，并实现插入、查找和遍历操作。

**提示**: 定义 `tree-node` 记录，实现 BST 算法。

<details>
<summary>查看答案</summary>

```scheme
(define-record-type tree-node
  (fields value (mutable left) (mutable right))
  (protocol
   (lambda (new)
     (case-lambda
       [(value) (new value #f #f)]
       [(value left right) (new value left right)]))))

;; 插入
(define (insert tree value)
  (cond
    [(not tree) (make-tree-node value)]
    [(< value (tree-node-value tree))
     (tree-node-left-set! tree (insert (tree-node-left tree) value))
     tree]
    [(> value (tree-node-value tree))
     (tree-node-right-set! tree (insert (tree-node-right tree) value))
     tree]
    [else tree]))

;; 查找
(define (search tree value)
  (cond
    [(not tree) #f]
    [(= value (tree-node-value tree)) #t]
    [(< value (tree-node-value tree))
     (search (tree-node-left tree) value)]
    [else
     (search (tree-node-right tree) value)]))

;; 中序遍历
(define (inorder tree)
  (if tree
      (append (inorder (tree-node-left tree))
              (list (tree-node-value tree))
              (inorder (tree-node-right tree)))
      '()))

;; 测试
(define tree #f)
(set! tree (insert tree 5))
(set! tree (insert tree 3))
(set! tree (insert tree 7))
(set! tree (insert tree 1))
(set! tree (insert tree 9))
(inorder tree)  ; => (1 3 5 7 9)
(search tree 7)  ; => #t
(search tree 4)  ; => #f
```
</details>

---

### 练习 7: 栈和队列

**问题**: 使用记录类型实现栈和队列数据结构。

<details>
<summary>查看答案</summary>

```scheme
;; 栈
(define-record-type stack
  (fields (mutable items))
  (protocol
   (lambda (new)
     (lambda () (new '())))))

(define (push! stk item)
  (stack-items-set! stk (cons item (stack-items stk))))

(define (pop! stk)
  (when (null? (stack-items stk))
    (error 'pop! "stack is empty"))
  (let ([item (car (stack-items stk))])
    (stack-items-set! stk (cdr (stack-items stk)))
    item))

(define (stack-empty? stk)
  (null? (stack-items stk)))

;; 队列
(define-record-type queue
  (fields (mutable front) (mutable rear))
  (protocol
   (lambda (new)
     (lambda () (new '() '())))))

(define (enqueue! q item)
  (queue-rear-set! q (cons item (queue-rear q))))

(define (dequeue! q)
  (when (and (null? (queue-front q)) (null? (queue-rear q)))
    (error 'dequeue! "queue is empty"))
  (when (null? (queue-front q))
    (queue-front-set! q (reverse (queue-rear q)))
    (queue-rear-set! q '()))
  (let ([item (car (queue-front q))])
    (queue-front-set! q (cdr (queue-front q)))
    item))

(define (queue-empty? q)
  (and (null? (queue-front q)) (null? (queue-rear q))))

;; 测试
(define s (make-stack))
(push! s 1)
(push! s 2)
(push! s 3)
(pop! s)  ; => 3

(define q (make-queue))
(enqueue! q 1)
(enqueue! q 2)
(enqueue! q 3)
(dequeue! q)  ; => 1
```
</details>

---

## Continuation 练习

### 练习 8: 生成器

**问题**: 使用 `call/cc` 实现一个生成从 start 到 end 的整数生成器。

<details>
<summary>查看答案</summary>

```scheme
(define (make-range-generator start end)
  (let ([current start]
        [return #f])
    (lambda ()
      (call/cc
       (lambda (k)
         (set! return k)
         (when (>= current end)
           (return 'done))
         (let ([value current])
           (set! current (+ current 1))
           value))))))

;; 使用
(define gen (make-range-generator 0 5))
(gen)  ; => 0
(gen)  ; => 1
(gen)  ; => 2
(gen)  ; => 3
(gen)  ; => 4
(gen)  ; => done
```
</details>

---

### 练习 9: 协程

**问题**: 实现两个协程，相互传递控制权。

<details>
<summary>查看答案</summary>

```scheme
(define (make-coroutine proc)
  (let ([return #f]
        [resume #f])
    (define (yield value)
      (call/cc
       (lambda (k)
         (set! resume k)
         (return value))))
    (lambda (value)
      (call/cc
       (lambda (k)
         (set! return k)
         (if resume
             (resume value)
             (begin
               (proc yield)
               'done)))))))

;; 使用示例
(define producer
  (make-coroutine
   (lambda (yield)
     (let loop ([i 1])
       (when (<= i 5)
         (yield i)
         (loop (+ i 1)))))))

(define consumer
  (make-coroutine
   (lambda (yield)
     (let loop ()
       (let ([val (yield #f)])
         (unless (eq? val 'done)
           (display "Received: ")
           (display val)
           (newline)
           (loop)))))))

;; 运行
(let loop ()
  (let ([val (producer #f)])
    (unless (eq? val 'done)
      (consumer val)
      (loop))))
```
</details>

---

## 性能优化练习

### 练习 10: 斐波那契优化

**问题**: 实现三个版本的斐波那契函数：
1. 朴素递归
2. 记忆化
3. 迭代（尾递归）

比较它们的性能。

<details>
<summary>查看答案</summary>

```scheme
;; 1. 朴素递归（慢）
(define (fib-naive n)
  (if (<= n 1)
      n
      (+ (fib-naive (- n 1))
         (fib-naive (- n 2)))))

;; 2. 记忆化
(define fib-memo
  (let ([memo (make-eq-hashtable)])
    (lambda (n)
      (or (hashtable-ref memo n #f)
          (let ([result (if (<= n 1)
                            n
                            (+ (fib-memo (- n 1))
                               (fib-memo (- n 2))))])
            (hashtable-set! memo n result)
            result)))))

;; 3. 迭代（最快）
(define (fib-iter n)
  (let loop ([a 0] [b 1] [count n])
    (if (= count 0)
        a
        (loop b (+ a b) (- count 1)))))

;; 性能测试
(define (benchmark proc n iterations)
  (collect)
  (let ([start (current-time)])
    (do ([i 0 (+ i 1)])
        ((>= i iterations))
      (proc n))
    (let ([end (current-time)])
      (- (time-second end) (time-second start)))))

;; 测试
(display "fib(30) - 朴素递归: ")
(display (benchmark fib-naive 30 1))
(display " 秒\n")

(display "fib(30) - 记忆化: ")
(display (benchmark fib-memo 30 1000))
(display " 秒\n")

(display "fib(30) - 迭代: ")
(display (benchmark fib-iter 30 10000))
(display " 秒\n")
```
</details>

---

## 项目练习

### 练习 11: 简单的解释器

**问题**: 实现一个简单的算术表达式解释器，支持 `+`, `-`, `*`, `/` 和括号。

**示例**:
```scheme
(eval-expr '(+ 1 2))           ; => 3
(eval-expr '(* (+ 1 2) 3))     ; => 9
(eval-expr '(/ (- 10 2) 2))    ; => 4
```

<details>
<summary>查看答案</summary>

```scheme
(define (eval-expr expr)
  (cond
    [(number? expr) expr]
    [(not (pair? expr)) (error 'eval-expr "invalid expression" expr)]
    [else
     (let ([op (car expr)]
           [args (map eval-expr (cdr expr))])
       (case op
         [(+) (apply + args)]
         [(-) (apply - args)]
         [(*) (apply * args)]
         [(/) (apply / args)]
         [else (error 'eval-expr "unknown operator" op)]))]))

;; 测试
(eval-expr '(+ 1 2))              ; => 3
(eval-expr '(* (+ 1 2) 3))        ; => 9
(eval-expr '(/ (- 10 2) 2))       ; => 4
(eval-expr '(+ (* 2 3) (- 5 1)))  ; => 10
```
</details>

---

### 练习 12: 简单的数据库

**问题**: 实现一个简单的内存数据库，支持插入、查询和更新操作。

<details>
<summary>查看答案</summary>

```scheme
(define-record-type database
  (fields (mutable tables))
  (protocol
   (lambda (new)
     (lambda ()
       (new (make-eq-hashtable))))))

(define (create-table! db table-name)
  (hashtable-set! (database-tables db) table-name '()))

(define (insert! db table-name record)
  (let ([table (hashtable-ref (database-tables db) table-name #f)])
    (unless table
      (error 'insert! "table not found" table-name))
    (hashtable-set! (database-tables db) table-name
                    (cons record table))))

(define (select db table-name predicate)
  (let ([table (hashtable-ref (database-tables db) table-name #f)])
    (unless table
      (error 'select "table not found" table-name))
    (filter predicate table)))

(define (update! db table-name predicate updater)
  (let ([table (hashtable-ref (database-tables db) table-name #f)])
    (unless table
      (error 'update! "table not found" table-name))
    (hashtable-set! (database-tables db) table-name
                    (map (lambda (record)
                           (if (predicate record)
                               (updater record)
                               record))
                         table))))

;; 测试
(define db (make-database))
(create-table! db 'users)

(insert! db 'users '((name . "Alice") (age . 30)))
(insert! db 'users '((name . "Bob") (age . 25)))
(insert! db 'users '((name . "Charlie") (age . 35)))

(select db 'users 
        (lambda (user) (> (cdr (assoc 'age user)) 28)))
;; => (((name . "Charlie") (age . 35)) ((name . "Alice") (age . 30)))
```
</details>

---

## 挑战练习

### 练习 13: Y 组合子

**问题**: 实现 Y 组合子，并用它定义阶乘函数。

<details>
<summary>查看答案</summary>

```scheme
;; Y 组合子（不能直接在 Scheme 中工作，因为立即求值）
;; 使用 Z 组合子（适用于应用序）
(define Z
  (lambda (f)
    ((lambda (x) (f (lambda (y) ((x x) y))))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

;; 使用 Z 组合子定义阶乘
(define fact
  (Z (lambda (f)
       (lambda (n)
         (if (<= n 1)
             1
             (* n (f (- n 1))))))))

(fact 5)  ; => 120

;; 或者使用 letrec 内部定义
(define Y
  (lambda (f)
    (letrec ([g (f (lambda (x) (g x)))])
      g)))

(define fact2
  (Y (lambda (f)
       (lambda (n)
         (if (<= n 1)
             1
             (* n (f (- n 1))))))))

(fact2 6)  ; => 720
```
</details>

---

### 练习 14: 惰性求值

**问题**: 实现惰性求值的无限流。

<details>
<summary>查看答案</summary>

```scheme
;; 延迟和强制
(define-syntax delay
  (syntax-rules ()
    [(_ expr) (lambda () expr)]))

(define (force promise)
  (promise))

;; 流构造
(define-syntax cons-stream
  (syntax-rules ()
    [(_ a b) (cons a (delay b))]))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

;; 无限整数流
(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))

(define integers (integers-from 1))

;; 流操作
(define (stream-take n stream)
  (if (= n 0)
      '()
      (cons (stream-car stream)
            (stream-take (- n 1) (stream-cdr stream)))))

(define (stream-filter pred stream)
  (cond
    [(pred (stream-car stream))
     (cons-stream (stream-car stream)
                  (stream-filter pred (stream-cdr stream)))]
    [else
     (stream-filter pred (stream-cdr stream))]))

;; 测试
(stream-take 10 integers)
;; => (1 2 3 4 5 6 7 8 9 10)

(stream-take 10 (stream-filter even? integers))
;; => (2 4 6 8 10 12 14 16 18 20)

;; 斐波那契流
(define fibs
  (letrec ([fib-stream
            (lambda (a b)
              (cons-stream a (fib-stream b (+ a b))))])
    (fib-stream 0 1)))

(stream-take 10 fibs)
;; => (0 1 1 2 3 5 8 13 21 34)
```
</details>

---

## 总结

这些练习涵盖了 Chez Scheme 的主要概念：

1. **基础**: 列表操作、高阶函数、递归
2. **宏**: syntax-rules 和 syntax-case
3. **记录**: 数据结构设计
4. **Continuation**: call/cc 的应用
5. **性能**: 优化技巧
6. **项目**: 实际应用

建议按顺序完成这些练习，并尝试自己实现答案后再查看参考答案。祝学习愉快！
