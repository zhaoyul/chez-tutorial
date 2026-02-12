#!/usr/bin/env scheme-script
;; 性能优化示例

;; 1. Fixnum vs 通用算术
(define (demonstrate-fixnum-ops)
  (display "=== Fixnum 操作优化 ===\n\n")
  
  ;; 通用版本
  (define (sum-generic n)
    (let loop ([i 0] [sum 0])
      (if (>= i n)
          sum
          (loop (+ i 1) (+ sum i)))))
  
  ;; Fixnum 版本
  (define (sum-fixnum n)
    (let loop ([i 0] [sum 0])
      (if (fx>=? i n)
          sum
          (loop (fx+ i 1) (fx+ sum i)))))
  
  (display "计算 0 到 999999 的和:\n")
  (display "  通用版本: ")
  (time (sum-generic 1000000))
  (display "  Fixnum版本: ")
  (time (sum-fixnum 1000000))
  
  (newline))

;; 2. 向量 vs 列表
(define (demonstrate-vector-vs-list)
  (display "=== 向量 vs 列表性能 ===\n\n")
  
  ;; 列表版本
  (define (sum-list lst)
    (let loop ([lst lst] [sum 0])
      (if (null? lst)
          sum
          (loop (cdr lst) (+ sum (car lst))))))
  
  ;; 向量版本
  (define (sum-vector vec)
    (let ([len (vector-length vec)])
      (let loop ([i 0] [sum 0])
        (if (fx>=? i len)
            sum
            (loop (fx+ i 1) (+ sum (vector-ref vec i)))))))
  
  (define test-list (let loop ([i 0] [acc '()])
                       (if (>= i 10000)
                           acc
                           (loop (+ i 1) (cons i acc)))))
  
  (define test-vector (make-vector 10000 0))
  (do ([i 0 (+ i 1)])
      ((>= i 10000))
    (vector-set! test-vector i i))
  
  (display "计算 10000 个元素的和:\n")
  (display "  列表版本: ")
  (time (sum-list test-list))
  (display "  向量版本: ")
  (time (sum-vector test-vector))
  
  (newline))

;; 3. 尾递归优化
(define (demonstrate-tail-recursion)
  (display "=== 尾递归优化 ===\n\n")
  
  ;; 非尾递归（会爆栈）
  (define (factorial-non-tail n)
    (if (<= n 1)
        1
        (* n (factorial-non-tail (- n 1)))))
  
  ;; 尾递归版本
  (define (factorial-tail n)
    (let loop ([n n] [acc 1])
      (if (<= n 1)
          acc
          (loop (- n 1) (* n acc)))))
  
  (display "计算阶乘:\n")
  (display "  非尾递归 factorial(10): ")
  (display (factorial-non-tail 10))
  (newline)
  (display "  尾递归   factorial(10): ")
  (display (factorial-tail 10))
  (newline)
  
  ;; 尾递归可以处理更大的数字
  (display "\n  尾递归可以计算更大的数: factorial(1000)\n")
  (display "  （结果太大不显示）\n")
  ;; (display (factorial-tail 1000)) ; 结果太大
  
  (newline))

;; 4. 内联函数
(define (demonstrate-inlining)
  (display "=== 内联函数 ===\n\n")
  
  ;; 普通函数
  (define (square x) (* x x))
  
  ;; 使用 define-integrable 强制内联
  (define-integrable (square-inline x) (* x x))
  
  (define (sum-of-squares-normal n)
    (let loop ([i 0] [sum 0])
      (if (>= i n)
          sum
          (loop (+ i 1) (+ sum (square i))))))
  
  (define (sum-of-squares-inline n)
    (let loop ([i 0] [sum 0])
      (if (>= i n)
          sum
          (loop (+ i 1) (+ sum (square-inline i))))))
  
  (display "计算平方和:\n")
  (display "  普通函数: ")
  (time (sum-of-squares-normal 100000))
  (display "  内联函数: ")
  (time (sum-of-squares-inline 100000))
  
  (newline))

;; 5. 避免重复计算
(define (demonstrate-memoization)
  (display "=== 记忆化（避免重复计算）===\n\n")
  
  ;; 普通的斐波那契（指数时间）
  (define (fib n)
    (if (<= n 1)
        n
        (+ (fib (- n 1)) (fib (- n 2)))))
  
  ;; 记忆化版本
  (define make-memoized-fib
    (let ([memo (make-eq-hashtable)])
      (lambda ()
        (define (fib-memo n)
          (or (hashtable-ref memo n #f)
              (let ([result (if (<= n 1)
                                n
                                (+ (fib-memo (- n 1))
                                   (fib-memo (- n 2))))])
                (hashtable-set! memo n result)
                result)))
        fib-memo)))
  
  (define fib-memo (make-memoized-fib))
  
  (display "计算斐波那契数列:\n")
  (display "  fib(30) - 普通版本: ")
  (time (fib 30))
  (display "  fib(30) - 记忆化版本: ")
  (time (fib-memo 30))
  
  (display "\n  fib(35) - 普通版本会很慢，跳过\n")
  (display "  fib(35) - 记忆化版本: ")
  (time (fib-memo 35))
  
  (newline))

;; 6. 局部变量缓存
(define (demonstrate-local-caching)
  (display "=== 局部变量缓存 ===\n\n")
  
  ;; 每次都访问全局变量
  (define global-list '(1 2 3 4 5))
  
  (define (process-global n)
    (let loop ([i 0])
      (when (< i n)
        (for-each (lambda (x) (* x x)) global-list)
        (loop (+ i 1)))))
  
  ;; 缓存到局部变量
  (define (process-cached n)
    (let ([cached-list global-list])
      (let loop ([i 0])
        (when (< i n)
          (for-each (lambda (x) (* x x)) cached-list)
          (loop (+ i 1))))))
  
  (display "处理列表 10000 次:\n")
  (display "  全局访问: ")
  (time (process-global 10000))
  (display "  局部缓存: ")
  (time (process-cached 10000))
  
  (newline))

;; 7. 预分配 vs 增量分配
(define (demonstrate-preallocation)
  (display "=== 预分配 vs 增量分配 ===\n\n")
  
  ;; 增量分配（使用 cons）
  (define (build-list-incremental n)
    (let loop ([i 0] [acc '()])
      (if (>= i n)
          (reverse acc)
          (loop (+ i 1) (cons i acc)))))
  
  ;; 预分配（使用向量）
  (define (build-vector-preallocated n)
    (let ([vec (make-vector n)])
      (do ([i 0 (+ i 1)])
          ((>= i n) vec)
        (vector-set! vec i i))))
  
  (display "构建 100000 个元素:\n")
  (display "  列表（增量分配）: ")
  (time (build-list-incremental 100000))
  (display "  向量（预分配）: ")
  (time (build-vector-preallocated 100000))
  
  (newline))

;; 8. 字符串操作优化
(define (demonstrate-string-ops)
  (display "=== 字符串操作优化 ===\n\n")
  
  ;; 多次拼接（低效）
  (define (concat-multiple-slow strs)
    (let loop ([strs strs] [result ""])
      (if (null? strs)
          result
          (loop (cdr strs) (string-append result (car strs))))))
  
  ;; 一次性拼接
  (define (concat-multiple-fast strs)
    (apply string-append strs))
  
  (define test-strings (let loop ([i 0] [acc '()])
                          (if (>= i 1000)
                              acc
                              (loop (+ i 1) (cons "test" acc)))))
  
  (display "拼接 1000 个字符串:\n")
  (display "  多次拼接: ")
  (time (concat-multiple-slow test-strings))
  (display "  一次拼接: ")
  (time (concat-multiple-fast test-strings))
  
  (newline))

;; 主函数
(define (main)
  (display "Chez Scheme 性能优化示例\n")
  (display "================================\n\n")
  
  (demonstrate-fixnum-ops)
  (demonstrate-vector-vs-list)
  (demonstrate-tail-recursion)
  (demonstrate-inlining)
  (demonstrate-memoization)
  (demonstrate-local-caching)
  (demonstrate-preallocation)
  (demonstrate-string-ops)
  
  (display "性能优化示例完成！\n\n")
  (display "提示：\n")
  (display "1. 使用 fixnum 操作符处理小整数\n")
  (display "2. 随机访问使用向量，顺序访问使用列表\n")
  (display "3. 保持尾递归形式\n")
  (display "4. 使用 define-integrable 内联小函数\n")
  (display "5. 使用记忆化避免重复计算\n")
  (display "6. 缓存频繁访问的全局变量\n")
  (display "7. 预分配内存而非增量分配\n")
  (display "8. 批量操作优于多次操作\n"))

;; 运行
(main)
