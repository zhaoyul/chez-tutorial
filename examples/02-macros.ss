#!/usr/bin/env scheme-script
;; 宏系统示例

;; 1. 简单的 syntax-rules 宏
(define-syntax when
  (syntax-rules ()
    [(_ test body ...)
     (if test (begin body ...))]))

;; 2. 带模式匹配的宏
(define-syntax my-let
  (syntax-rules ()
    [(_ ([var val] ...) body ...)
     ((lambda (var ...) body ...) val ...)]))

;; 3. 递归宏
(define-syntax my-and
  (syntax-rules ()
    [(_) #t]
    [(_ test) test]
    [(_ test rest ...)
     (if test (my-and rest ...) #f)]))

;; 4. 带字面量的宏
(define-syntax my-cond
  (syntax-rules (else)
    [(_ [else body ...])
     (begin body ...)]
    [(_ [test body ...] clause ...)
     (if test
         (begin body ...)
         (my-cond clause ...))]))

;; 5. 生成循环的宏
(define-syntax dotimes
  (syntax-rules ()
    [(_ (var n) body ...)
     (let loop ([var 0])
       (when (< var n)
         body ...
         (loop (+ var 1))))]))

;; 6. 断言宏
(define-syntax assert
  (syntax-rules ()
    [(_ test)
     (unless test
       (error 'assert "assertion failed" 'test))]
    [(_ test message)
     (unless test
       (error 'assert message))]))

;; 7. 调试宏
(define-syntax debug
  (syntax-rules ()
    [(_ expr)
     (let ([result expr])
       (display "DEBUG: ")
       (display 'expr)
       (display " => ")
       (display result)
       (newline)
       result)]))

;; 测试宏
(define (test-macros)
  (display "=== 宏系统示例 ===\n\n")
  
  ;; 测试 when
  (display "1. when 宏:\n")
  (when (> 5 3)
    (display "  5 大于 3\n"))
  
  ;; 测试 my-let
  (display "\n2. my-let 宏:\n")
  (my-let ([x 10] [y 20])
    (display "  x = ") (display x) (newline)
    (display "  y = ") (display y) (newline)
    (display "  x + y = ") (display (+ x y)) (newline))
  
  ;; 测试 my-and
  (display "\n3. my-and 宏:\n")
  (display "  (my-and #t #t #t): ")
  (display (my-and #t #t #t)) (newline)
  (display "  (my-and #t #f #t): ")
  (display (my-and #t #f #t)) (newline)
  
  ;; 测试 my-cond
  (display "\n4. my-cond 宏:\n")
  (let ([x 10])
    (display "  x = 10, 结果: ")
    (display
     (my-cond
       [(< x 0) "负数"]
       [(= x 0) "零"]
       [(> x 0) "正数"]
       [else "未知"]))
    (newline))
  
  ;; 测试 dotimes
  (display "\n5. dotimes 宏:\n")
  (display "  打印 0 到 4:\n  ")
  (dotimes (i 5)
    (display i) (display " "))
  (newline)
  
  ;; 测试 assert
  (display "\n6. assert 宏:\n")
  (assert (= (+ 1 1) 2))
  (display "  断言 (= (+ 1 1) 2) 通过\n")
  
  ;; 测试 debug
  (display "\n7. debug 宏:\n")
  (display "  ")
  (debug (+ 10 20 30))
  
  (newline))

;; 运行测试
(test-macros)

;; 更高级的宏示例

;; 8. 定义多个 getter/setter 的宏
(define-syntax define-accessors
  (syntax-rules ()
    [(_ type-name [field accessor mutator] ...)
     (begin
       (define (accessor obj)
         (type-name-field obj))
       (define (mutator obj val)
         (type-name-field-set! obj val))
       ...)]))

;; 9. 面向对象风格的宏
(define-syntax define-class
  (syntax-rules ()
    [(_ name (field ...) ([method-name method-body ...] ...))
     (begin
       (define-record-type name
         (fields field ...))
       (define (method-name obj . args)
         (let ([field (name-field obj)] ...)
           method-body ...))
       ...)]))

;; 10. 模式匹配宏（简化版）
(define-syntax match
  (syntax-rules (quote)
    [(_ val) (error 'match "no matching clause")]
    [(_ val ['quote literal] body ... rest ...)
     (if (equal? val 'literal)
         (begin body ...)
         (match val rest ...))]
    [(_ val [pattern guard body ...] rest ...)
     (let ([pattern val])
       (if guard
           (begin body ...)
           (match val rest ...)))]
    [(_ val [pattern body ...] rest ...)
     (let ([pattern val])
       (begin body ...))]))

(define (test-advanced-macros)
  (display "=== 高级宏示例 ===\n\n")
  
  ;; 测试 match
  (display "1. match 宏:\n")
  (let ([x 42])
    (display "  匹配数字 42: ")
    (match x
      [0 (display "零")]
      ['symbol (display "符号")]
      [n (positive? n) (display "正数")]
      [n (display "其他")])
    (newline))
  
  (newline))

(test-advanced-macros)
