#!/usr/bin/env scheme-script
;; Continuation 示例

;; 1. 基本的 call/cc 用法
(define (demonstrate-basic-callcc)
  (display "=== 基本 call/cc 示例 ===\n\n")
  
  ;; 简单的 continuation 捕获
  (display "1. 简单捕获:\n")
  (display "  结果: ")
  (display
   (call/cc
    (lambda (k)
      (k 42)
      (display "这行永远不会执行")
      99)))
  (newline)
  
  ;; 条件返回
  (display "\n2. 条件提前返回:\n")
  (define (find-first pred lst)
    (call/cc
     (lambda (return)
       (for-each
        (lambda (x)
          (when (pred x)
            (return x)))
        lst)
       #f)))
  
  (display "  在 '(1 3 5 6 7 9) 中找到第一个偶数: ")
  (display (find-first even? '(1 3 5 6 7 9)))
  (newline)
  
  (newline))

;; 2. 非局部退出
(define (demonstrate-nonlocal-exit)
  (display "=== 非局部退出示例 ===\n\n")
  
  ;; 从嵌套循环中退出
  (define (find-pair pred lst1 lst2)
    (call/cc
     (lambda (return)
       (for-each
        (lambda (x)
          (for-each
           (lambda (y)
             (when (pred x y)
               (return (cons x y))))
           lst2))
        lst1)
       #f)))
  
  (display "找到满足 x + y = 10 的数对:\n")
  (let ([pair (find-pair (lambda (x y) (= (+ x y) 10))
                         '(1 2 3 4 5)
                         '(5 6 7 8 9))])
    (display "  结果: ")
    (if pair
        (begin
          (display "(")
          (display (car pair))
          (display ", ")
          (display (cdr pair))
          (display ")\n"))
        (display "未找到\n")))
  
  (newline))

;; 3. 生成器
(define (demonstrate-generators)
  (display "=== 生成器示例 ===\n\n")
  
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
  
  (display "生成器生成 0-5:\n  ")
  (define gen (make-range-generator 0 5))
  (let loop ()
    (let ([val (gen)])
      (unless (eq? val 'done)
        (display val)
        (display " ")
        (loop))))
  (newline)
  
  (newline))

;; 4. 协程（Coroutines）
(define (demonstrate-coroutines)
  (display "=== 协程示例 ===\n\n")
  
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
  
  (display "协程：生产者-消费者模式\n")
  (define producer
    (make-coroutine
     (lambda (yield)
       (let loop ([i 1])
         (when (<= i 5)
           (yield i)
           (loop (+ i 1)))))))
  
  (display "  生产: ")
  (let loop ()
    (let ([val (producer #f)])
      (unless (eq? val 'done)
        (display val)
        (display " ")
        (loop))))
  (newline)
  
  (newline))

;; 5. 异常处理的实现
(define (demonstrate-exception-handling)
  (display "=== 使用 call/cc 实现异常处理 ===\n\n")
  
  (define *handlers* '())
  
  (define (push-handler! handler)
    (set! *handlers* (cons handler *handlers*)))
  
  (define (pop-handler!)
    (set! *handlers* (cdr *handlers*)))
  
  (define (throw exception)
    (if (null? *handlers*)
        (error 'throw "未捕获的异常" exception)
        ((car *handlers*) exception)))
  
  (define-syntax try
    (syntax-rules (catch)
      [(_ body (catch handler))
       (call/cc
        (lambda (k)
          (push-handler! (lambda (ex) (k (handler ex))))
          (let ([result body])
            (pop-handler!)
            result)))]))
  
  (display "示例 1: 捕获异常\n")
  (display "  结果: ")
  (display
   (try
    (begin
      (display "尝试执行...")
      (throw "错误!")
      (display "这行不会执行")
      'success)
    (catch (lambda (ex)
             (display "捕获到异常: ")
             (display ex)
             'failed))))
  (newline)
  
  (newline))

;; 6. 回溯搜索
(define (demonstrate-backtracking)
  (display "=== 回溯搜索示例 ===\n\n")
  
  ;; 简单的回溯框架
  (define *paths* '())
  
  (define (mark-choice)
    (call/cc
     (lambda (k)
       (set! *paths* (cons k *paths*))
       #f)))
  
  (define (backtrack)
    (if (null? *paths*)
        (error 'backtrack "no more choices")
        (let ([k (car *paths*)])
          (set! *paths* (cdr *paths*))
          (k #t))))
  
  (define (choose-from lst)
    (if (null? lst)
        (backtrack)
        (let ([choice (car lst)])
          (if (mark-choice)
              (choose-from (cdr lst))
              choice))))
  
  (display "在列表中选择两个数字，使其和为 10:\n")
  (call/cc
   (lambda (exit)
     (set! *paths* '())
     (let ([x (choose-from '(1 2 3 4 5 6 7 8 9))]
           [y (choose-from '(1 2 3 4 5 6 7 8 9))])
       (if (= (+ x y) 10)
           (begin
             (display "  找到: ")
             (display x)
             (display " + ")
             (display y)
             (display " = 10\n")
             (exit #t))
           (backtrack)))))
  
  (newline))

;; 7. 时间旅行调试器（概念演示）
(define (demonstrate-time-travel)
  (display "=== 时间旅行概念 ===\n\n")
  
  (define snapshots '())
  
  (define (take-snapshot)
    (call/cc
     (lambda (k)
       (set! snapshots (cons k snapshots))
       'snapshot-taken)))
  
  (define (restore-snapshot n)
    (if (< n (length snapshots))
        (begin
          (display "恢复到快照 ")
          (display n)
          (newline)
          ((list-ref snapshots n) 'restored))
        (error 'restore-snapshot "invalid snapshot")))
  
  (display "程序执行过程:\n")
  (let ([counter 0])
    (define (step msg)
      (set! counter (+ counter 1))
      (display "  步骤 ")
      (display counter)
      (display ": ")
      (display msg)
      (newline)
      (when (= counter 3)
        (take-snapshot)))
    
    (step "初始化")
    (step "加载数据")
    (step "处理数据")  ; 在这里拍快照
    (step "保存结果")
    
    ;; 注意：实际使用需要更复杂的状态管理
    (display "\n（在真实实现中，这里可以恢复到快照）\n"))
  
  (newline))

;; 主函数
(define (main)
  (demonstrate-basic-callcc)
  (demonstrate-nonlocal-exit)
  (demonstrate-generators)
  (demonstrate-coroutines)
  (demonstrate-exception-handling)
  (demonstrate-backtracking)
  (demonstrate-time-travel)
  (display "Continuation 示例完成！\n"))

;; 运行
(main)
