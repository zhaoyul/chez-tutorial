#!/usr/bin/env scheme-script
;; 实用工具集示例

;; 1. 命令行参数解析
(define (demonstrate-command-line-args)
  (display "=== 命令行参数 ===\n\n")
  
  (display "程序名: ")
  (display (car (command-line)))
  (newline)
  
  (display "参数列表: ")
  (display (cdr (command-line)))
  (newline)
  
  (display "参数数量: ")
  (display (length (cdr (command-line))))
  (newline)
  
  (newline))

;; 2. 环境变量
(define (demonstrate-environment-variables)
  (display "=== 环境变量 ===\n\n")
  
  (display "获取环境变量:\n")
  (let ([home (getenv "HOME")]
        [path (getenv "PATH")]
        [user (getenv "USER")])
    (display "  HOME: ") (display (or home "未设置")) (newline)
    (display "  USER: ") (display (or user "未设置")) (newline)
    (when path
      (display "  PATH 长度: ") (display (string-length path)) (newline)))
  
  (newline))

;; 3. 时间和日期处理
(define (demonstrate-time-date)
  (display "=== 时间和日期 ===\n\n")
  
  (display "1. 当前时间:\n")
  (let ([now (current-date)])
    (display "  年: ") (display (date-year now)) (newline)
    (display "  月: ") (display (date-month now)) (newline)
    (display "  日: ") (display (date-day now)) (newline)
    (display "  时: ") (display (date-hour now)) (newline)
    (display "  分: ") (display (date-minute now)) (newline)
    (display "  秒: ") (display (date-second now)) (newline))
  
  (display "\n2. 时间格式化:\n")
  (let ([now (current-date)])
    (display "  ")
    (display (format-date now)))
  (newline)
  
  (display "\n3. 时间测量:\n")
  (let ([start (current-time)])
    (do ([i 0 (+ i 1)])
        ((>= i 1000000)))
    (let ([end (current-time)])
      (display "  循环 1,000,000 次耗时: ")
      (display (time-difference end start))
      (display " 秒\n")))
  
  (newline))

;; 格式化日期
(define (format-date date)
  (format "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
          (date-year date)
          (date-month date)
          (date-day date)
          (date-hour date)
          (date-minute date)
          (date-second date)))

;; 计算时间差
(define (time-difference end start)
  (let ([diff-sec (- (time-second end) (time-second start))]
        [diff-nano (- (time-nanosecond end) (time-nanosecond start))])
    (+ diff-sec (/ diff-nano 1000000000.0))))

;; 4. 随机数生成
(define (demonstrate-random)
  (display "=== 随机数生成 ===\n\n")
  
  (display "生成 10 个随机数:\n  ")
  (do ([i 0 (+ i 1)])
      ((>= i 10))
    (display (random 100))
    (display " "))
  (newline)
  
  (display "\n生成随机浮点数 (0.0 - 1.0):\n  ")
  (do ([i 0 (+ i 1)])
      ((>= i 5))
    (display (/ (random 1000) 1000.0))
    (display " "))
  (newline)
  
  (newline))

;; 5. 哈希表操作
(define (demonstrate-hashtable)
  (display "=== 哈希表操作 ===\n\n")
  
  (display "1. 创建和使用哈希表:\n")
  (let ([ht (make-eq-hashtable)])
    (hashtable-set! ht 'name "张三")
    (hashtable-set! ht 'age 30)
    (hashtable-set! ht 'city "北京")
    
    (display "  姓名: ") 
    (display (hashtable-ref ht 'name "未知"))
    (newline)
    
    (display "  年龄: ")
    (display (hashtable-ref ht 'age 0))
    (newline)
    
    (display "  城市: ")
    (display (hashtable-ref ht 'city "未知"))
    (newline)
    
    (display "  大小: ")
    (display (hashtable-size ht))
    (newline))
  
  (display "\n2. 字符串键哈希表:\n")
  (let ([ht (make-hashtable string-hash string=?)])
    (hashtable-set! ht "apple" 5)
    (hashtable-set! ht "banana" 3)
    (hashtable-set! ht "orange" 7)
    
    (display "  apple: ")
    (display (hashtable-ref ht "apple" 0))
    (newline)
    
    (display "  遍历哈希表:\n")
    (let-values ([(keys values) (hashtable-entries ht)])
      (vector-for-each
       (lambda (key value)
         (display "    ")
         (display key)
         (display " => ")
         (display value)
         (newline))
       keys
       values)))
  
  (newline))

;; 6. 字符串操作工具
(define (demonstrate-string-utilities)
  (display "=== 字符串工具 ===\n\n")
  
  (let ([str "  Hello, World!  "])
    (display "原始字符串: '") (display str) (display "'\n")
    (display "去空白: '") (display (string-trim str)) (display "'\n"))
  
  (display "\n字符串拼接:\n")
  (let ([parts '("Chez" "Scheme" "Tutorial")])
    (display "  用空格连接: ")
    (display (string-join parts " "))
    (newline)
    (display "  用 - 连接: ")
    (display (string-join parts "-"))
    (newline))
  
  (display "\n字符串分割:\n")
  (let ([text "apple,banana,orange"])
    (display "  原始: ") (display text) (newline)
    (display "  分割: ") (display (string-split text #\,)) (newline))
  
  (display "\n大小写转换:\n")
  (let ([text "Hello World"])
    (display "  原始: ") (display text) (newline)
    (display "  大写: ") (display (string-upcase text)) (newline)
    (display "  小写: ") (display (string-downcase text)) (newline))
  
  (newline))

;; 字符串连接
(define (string-join strs delimiter)
  (if (null? strs)
      ""
      (let loop ([strs (cdr strs)] [result (car strs)])
        (if (null? strs)
            result
            (loop (cdr strs)
                  (string-append result delimiter (car strs)))))))

;; 字符串分割
(define (string-split str delim)
  (let ([len (string-length str)])
    (let loop ([start 0] [result '()])
      (if (>= start len)
          (reverse result)
          (let ([pos (string-index str delim start)])
            (if pos
                (loop (+ pos 1)
                      (cons (substring str start pos) result))
                (reverse (cons (substring str start len) result))))))))

;; 字符串查找
(define (string-index str char start)
  (let ([len (string-length str)])
    (let loop ([i start])
      (cond
        [(>= i len) #f]
        [(char=? (string-ref str i) char) i]
        [else (loop (+ i 1))]))))

;; 字符串去空白
(define (string-trim str)
  (let ([len (string-length str)])
    (let ([start (let loop ([i 0])
                   (if (or (>= i len)
                           (not (char-whitespace? (string-ref str i))))
                       i
                       (loop (+ i 1))))]
          [end (let loop ([i (- len 1)])
                 (if (or (< i 0)
                         (not (char-whitespace? (string-ref str i))))
                     (+ i 1)
                     (loop (- i 1))))])
      (if (> start end)
          ""
          (substring str start end)))))

;; 7. 列表和向量转换
(define (demonstrate-conversions)
  (display "=== 数据结构转换 ===\n\n")
  
  (let ([lst '(1 2 3 4 5)])
    (display "列表: ") (display lst) (newline)
    (display "转为向量: ") (display (list->vector lst)) (newline))
  
  (let ([vec (vector 'a 'b 'c 'd 'e)])
    (display "\n向量: ") (write vec) (newline)
    (display "转为列表: ") (display (vector->list vec)) (newline))
  
  (let ([str "Hello"])
    (display "\n字符串: ") (display str) (newline)
    (display "转为列表: ") (display (string->list str)) (newline))
  
  (let ([chars '(#\H #\e #\l #\l #\o)])
    (display "\n字符列表: ") (display chars) (newline)
    (display "转为字符串: ") (display (list->string chars)) (newline))
  
  (newline))

;; 8. 排序和搜索
(define (demonstrate-sort-search)
  (display "=== 排序和搜索 ===\n\n")
  
  (let ([numbers '(5 2 8 1 9 3 7 4 6)])
    (display "原始列表: ") (display numbers) (newline)
    (display "排序后: ") (display (list-sort < numbers)) (newline)
    (display "降序: ") (display (list-sort > numbers)) (newline))
  
  (let ([words '("banana" "apple" "orange" "grape")])
    (display "\n字符串列表: ") (display words) (newline)
    (display "排序后: ") (display (list-sort string<? words)) (newline))
  
  (let ([lst '(1 2 3 4 5 6 7 8 9 10)])
    (display "\n查找元素:\n")
    (display "  查找 5: ") (display (member 5 lst)) (newline)
    (display "  查找 11: ") (display (member 11 lst)) (newline))
  
  (newline))

;; 9. 实用的高阶函数
(define (demonstrate-higher-order-utilities)
  (display "=== 实用高阶函数 ===\n\n")
  
  (display "1. partition (分区):\n")
  (let ([numbers '(1 2 3 4 5 6 7 8 9 10)])
    (let-values ([(evens odds) (partition even? numbers)])
      (display "  偶数: ") (display evens) (newline)
      (display "  奇数: ") (display odds) (newline)))
  
  (display "\n2. take 和 drop:\n")
  (let ([lst '(1 2 3 4 5 6 7 8 9 10)])
    (display "  原始: ") (display lst) (newline)
    (display "  前 5 个: ") (display (take lst 5)) (newline)
    (display "  跳过前 5 个: ") (display (drop lst 5)) (newline))
  
  (display "\n3. zip (配对):\n")
  (let ([names '("Alice" "Bob" "Charlie")]
        [ages '(25 30 35)])
    (display "  姓名: ") (display names) (newline)
    (display "  年龄: ") (display ages) (newline)
    (display "  配对: ") (display (zip names ages)) (newline))
  
  (newline))

;; partition 实现
(define (partition pred lst)
  (let loop ([lst lst] [true-list '()] [false-list '()])
    (cond
      [(null? lst) (values (reverse true-list) (reverse false-list))]
      [(pred (car lst))
       (loop (cdr lst) (cons (car lst) true-list) false-list)]
      [else
       (loop (cdr lst) true-list (cons (car lst) false-list))])))

;; take 实现
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;; drop 实现
(define (drop lst n)
  (if (or (null? lst) (<= n 0))
      lst
      (drop (cdr lst) (- n 1))))

;; zip 实现
(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (cons (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

;; 主函数
(define (main)
  (display "Chez Scheme 实用工具示例\n")
  (display "==========================\n\n")
  
  (demonstrate-command-line-args)
  (demonstrate-environment-variables)
  (demonstrate-time-date)
  (demonstrate-random)
  (demonstrate-hashtable)
  (demonstrate-string-utilities)
  (demonstrate-conversions)
  (demonstrate-sort-search)
  (demonstrate-higher-order-utilities)
  
  (display "实用工具示例完成！\n"))

;; 运行
(main)
