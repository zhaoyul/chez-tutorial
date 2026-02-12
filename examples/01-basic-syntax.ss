#!/usr/bin/env scheme-script
;; 基础语法示例

;; 1. 数据类型示例
(define (demonstrate-data-types)
  (display "=== 数据类型示例 ===\n")
  
  ;; 数值
  (display "整数: ") (display 42) (newline)
  (display "有理数: ") (display 1/3) (newline)
  (display "复数: ") (display 3+4i) (newline)
  (display "浮点数: ") (display 3.14) (newline)
  
  ;; 字符串
  (display "字符串: ") (display "Hello, 世界") (newline)
  
  ;; 符号
  (display "符号: ") (display 'symbol) (newline)
  
  ;; 列表
  (display "列表: ") (display '(1 2 3 4 5)) (newline)
  
  ;; 向量
  (display "向量: ") (display #(1 2 3 4 5)) (newline)
  
  (newline))

;; 2. 控制流示例
(define (demonstrate-control-flow)
  (display "=== 控制流示例 ===\n")
  
  ;; if 表达式
  (display "if 表达式: ")
  (display (if (> 5 3) "5 > 3" "5 <= 3"))
  (newline)
  
  ;; cond 表达式
  (display "cond 表达式: ")
  (let ([x 10])
    (display
     (cond
       [(< x 0) "负数"]
       [(= x 0) "零"]
       [(> x 0) "正数"])))
  (newline)
  
  ;; case 表达式
  (display "case 表达式: ")
  (display
   (case 'b
     [(a) "找到 a"]
     [(b c) "找到 b 或 c"]
     [else "其他"]))
  (newline)
  
  (newline))

;; 3. 函数示例
(define (demonstrate-functions)
  (display "=== 函数示例 ===\n")
  
  ;; 简单函数
  (define (square x) (* x x))
  (display "平方函数 square(5): ") 
  (display (square 5))
  (newline)
  
  ;; 递归函数
  (define (factorial n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))))
  (display "阶乘 factorial(5): ")
  (display (factorial 5))
  (newline)
  
  ;; 尾递归优化
  (define (factorial-tail n)
    (let loop ([n n] [acc 1])
      (if (<= n 1)
          acc
          (loop (- n 1) (* n acc)))))
  (display "尾递归阶乘 factorial-tail(5): ")
  (display (factorial-tail 5))
  (newline)
  
  ;; 可变参数
  (define (sum . args)
    (apply + args))
  (display "可变参数求和 sum(1,2,3,4,5): ")
  (display (sum 1 2 3 4 5))
  (newline)
  
  ;; 闭包
  (define (make-counter)
    (let ([count 0])
      (lambda ()
        (set! count (+ count 1))
        count)))
  (define counter (make-counter))
  (display "闭包计数器: ")
  (display (counter)) (display " ")
  (display (counter)) (display " ")
  (display (counter))
  (newline)
  
  (newline))

;; 4. 列表操作示例
(define (demonstrate-list-operations)
  (display "=== 列表操作示例 ===\n")
  
  (define lst '(1 2 3 4 5))
  
  (display "原始列表: ") (display lst) (newline)
  
  (display "car (第一个元素): ") (display (car lst)) (newline)
  (display "cdr (剩余元素): ") (display (cdr lst)) (newline)
  (display "length (长度): ") (display (length lst)) (newline)
  (display "reverse (反转): ") (display (reverse lst)) (newline)
  
  (display "map (映射): ")
  (display (map (lambda (x) (* x x)) lst))
  (newline)
  
  (display "filter (过滤): ")
  (display (filter even? lst))
  (newline)
  
  (display "fold-left (左折叠): ")
  (display (fold-left + 0 lst))
  (newline)
  
  (newline))

;; 5. 高阶函数示例
(define (demonstrate-higher-order-functions)
  (display "=== 高阶函数示例 ===\n")
  
  ;; compose
  (define (compose f g)
    (lambda (x) (f (g x))))
  
  (define add1 (lambda (x) (+ x 1)))
  (define double (lambda (x) (* x 2)))
  (define add1-then-double (compose double add1))
  
  (display "组合函数 ((compose double add1) 5): ")
  (display (add1-then-double 5))
  (newline)
  
  ;; curry
  (define (curry f)
    (lambda (x)
      (lambda (y)
        (f x y))))
  
  (define curried-add (curry +))
  (define add5 (curried-add 5))
  
  (display "柯里化 ((curry +) 5 3): ")
  (display (add5 3))
  (newline)
  
  (newline))

;; 主函数
(define (main)
  (demonstrate-data-types)
  (demonstrate-control-flow)
  (demonstrate-functions)
  (demonstrate-list-operations)
  (demonstrate-higher-order-functions)
  (display "示例运行完成！\n"))

;; 运行主函数
(main)
