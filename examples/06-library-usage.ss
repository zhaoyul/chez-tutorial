#!/usr/bin/env scheme-script
;; 库使用示例

;; 导入我们创建的库
(import (mylib math))
(import (mylib string-utils))
(import (mylib stack))

;; 测试数学库
(define (test-math-library)
  (display "=== 数学库测试 ===\n\n")
  
  (display "阶乘:\n")
  (display "  factorial(5) = ") (display (factorial 5)) (newline)
  (display "  factorial(10) = ") (display (factorial 10)) (newline)
  
  (display "\n斐波那契:\n")
  (display "  fibonacci(10) = ") (display (fibonacci 10)) (newline)
  (display "  fibonacci(20) = ") (display (fibonacci 20)) (newline)
  
  (display "\n最大公约数:\n")
  (display "  gcd(48, 18) = ") (display (gcd-euclid 48 18)) (newline)
  (display "  gcd(100, 35) = ") (display (gcd-euclid 100 35)) (newline)
  
  (display "\n素数判断:\n")
  (display "  prime?(17) = ") (display (prime? 17)) (newline)
  (display "  prime?(18) = ") (display (prime? 18)) (newline)
  (display "  prime?(97) = ") (display (prime? 97)) (newline)
  
  (newline))

;; 测试字符串工具库
(define (test-string-utils)
  (display "=== 字符串工具库测试 ===\n\n")
  
  (display "字符串分割:\n")
  (let ([parts (string-split "hello,world,scheme" ",")])
    (display "  分割 'hello,world,scheme': ")
    (display parts)
    (newline))
  
  (display "\n字符串连接:\n")
  (display "  连接 '(\"a\" \"b\" \"c\"): ")
  (display (string-join '("a" "b" "c") "-"))
  (newline)
  
  (display "\n去除空白:\n")
  (display "  trim '  hello world  ': '")
  (display (string-trim "  hello world  "))
  (display "'\n")
  
  (display "\n字符串反转:\n")
  (display "  reverse 'hello': ")
  (display (string-reverse "hello"))
  (newline)
  
  (newline))

;; 测试栈数据结构
(define (test-stack)
  (display "=== 栈数据结构测试 ===\n\n")
  
  (define stk (make-stack))
  
  (display "创建空栈\n")
  (display "  empty? = ") (display (empty? stk)) (newline)
  
  (display "\n压入元素 1, 2, 3:\n")
  (push! stk 1)
  (push! stk 2)
  (push! stk 3)
  (display "  栈内容: ") (display (stack->list stk)) (newline)
  (display "  empty? = ") (display (empty? stk)) (newline)
  
  (display "\npeek:\n")
  (display "  栈顶元素: ") (display (peek stk)) (newline)
  
  (display "\npop:\n")
  (display "  弹出: ") (display (pop! stk)) (newline)
  (display "  弹出: ") (display (pop! stk)) (newline)
  (display "  栈内容: ") (display (stack->list stk)) (newline)
  
  (display "\n压入更多元素:\n")
  (push! stk 4)
  (push! stk 5)
  (display "  栈内容: ") (display (stack->list stk)) (newline)
  
  (newline))

;; 综合示例：使用多个库
(define (comprehensive-example)
  (display "=== 综合示例 ===\n\n")
  
  (display "找出1-50之间的所有素数:\n  ")
  (let loop ([n 2])
    (when (<= n 50)
      (when (prime? n)
        (display n)
        (display " "))
      (loop (+ n 1))))
  (newline)
  
  (display "\n计算斐波那契数列的前10项:\n  ")
  (let loop ([i 0])
    (when (< i 10)
      (display (fibonacci i))
      (display " ")
      (loop (+ i 1))))
  (newline)
  
  (display "\n处理CSV数据:\n")
  (let ([csv-line "Alice,30,Engineer"])
    (display "  原始: ") (display csv-line) (newline)
    (display "  解析: ") (display (string-split csv-line ",")) (newline))
  
  (display "\n使用栈处理表达式:\n")
  (define stk (make-stack))
  (display "  表达式: 3 4 + 5 *\n")
  (push! stk 3)
  (display "  push 3: ") (display (stack->list stk)) (newline)
  (push! stk 4)
  (display "  push 4: ") (display (stack->list stk)) (newline)
  (let ([b (pop! stk)]
        [a (pop! stk)])
    (push! stk (+ a b))
    (display "  计算 +: ") (display (stack->list stk)) (newline))
  (push! stk 5)
  (display "  push 5: ") (display (stack->list stk)) (newline)
  (let ([b (pop! stk)]
        [a (pop! stk)])
    (push! stk (* a b))
    (display "  计算 *: ") (display (stack->list stk)) (newline))
  (display "  结果: ") (display (pop! stk)) (newline)
  
  (newline))

;; 主函数
(define (main)
  (display "Chez Scheme 库使用示例\n")
  (display "========================\n\n")
  
  (test-math-library)
  (test-string-utils)
  (test-stack)
  (comprehensive-example)
  
  (display "库使用示例完成！\n"))

;; 运行
(main)
