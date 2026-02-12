#!/usr/bin/env scheme-script
;; 文件 I/O 和字符串处理示例

;; 1. 基本文件读写
(define (demonstrate-basic-io)
  (display "=== 基本文件 I/O ===\n\n")
  
  ;; 写入文件
  (display "1. 写入文件:\n")
  (let ([filename "/tmp/test-output.txt"])
    (call-with-output-file filename
      (lambda (port)
        (display "Hello, Chez Scheme!\n" port)
        (display "这是第二行\n" port)
        (display "Line 3\n" port))
      'replace)
    (display "  已写入文件: ") (display filename) (newline))
  
  ;; 读取整个文件
  (display "\n2. 读取文件内容:\n")
  (let ([content (call-with-input-file "/tmp/test-output.txt"
                   (lambda (port)
                     (let loop ([lines '()])
                       (let ([line (get-line port)])
                         (if (eof-object? line)
                             (reverse lines)
                             (loop (cons line lines)))))))])
    (display "  读取到的行数: ") (display (length content)) (newline)
    (display "  内容:\n")
    (for-each (lambda (line)
                (display "    ") (display line) (newline))
              content))
  
  (newline))

;; 2. 逐行处理文件
(define (demonstrate-line-processing)
  (display "=== 逐行处理文件 ===\n\n")
  
  ;; 创建测试文件
  (call-with-output-file "/tmp/numbers.txt"
    (lambda (port)
      (do ([i 1 (+ i 1)])
          ((> i 10))
        (fprintf port "Line ~a: ~a\n" i (* i i))))
    'replace)
  
  (display "处理数字文件:\n")
  (call-with-input-file "/tmp/numbers.txt"
    (lambda (port)
      (let loop ([line-num 0] [sum 0])
        (let ([line (get-line port)])
          (if (eof-object? line)
              (begin
                (display "总行数: ") (display line-num) (newline)
                (display "数字总和: ") (display sum) (newline))
              (loop (+ line-num 1)
                    (+ sum (extract-number line))))))))
  
  (newline))

;; 辅助函数：从字符串中提取数字
(define (extract-number str)
  (let* ([parts (string-split str #\:)]
         [num-str (if (>= (length parts) 2)
                      (string-trim (cadr parts))
                      "0")])
    (string->number num-str)))

;; 字符串分割辅助函数
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

;; 字符串查找辅助函数
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

;; 3. 二进制文件处理
(define (demonstrate-binary-io)
  (display "=== 二进制文件 I/O ===\n\n")
  
  ;; 写入二进制数据
  (display "1. 写入二进制文件:\n")
  (let ([filename "/tmp/binary-data.bin"])
    (call-with-output-file filename
      (lambda (port)
        (let ([bv (make-bytevector 10)])
          (do ([i 0 (+ i 1)])
              ((>= i 10))
            (bytevector-u8-set! bv i (* i 10)))
          (put-bytevector port bv)))
      'replace)
    (display "  已写入二进制文件\n"))
  
  ;; 读取二进制数据
  (display "\n2. 读取二进制文件:\n")
  (call-with-input-file "/tmp/binary-data.bin"
    (lambda (port)
      (let ([bv (get-bytevector-all port)])
        (display "  字节数: ") (display (bytevector-length bv)) (newline)
        (display "  内容: ")
        (do ([i 0 (+ i 1)])
            ((>= i (bytevector-length bv)))
          (display (bytevector-u8-ref bv i))
          (display " "))
        (newline))))
  
  (newline))

;; 4. 文件操作
(define (demonstrate-file-operations)
  (display "=== 文件系统操作 ===\n\n")
  
  ;; 检查文件是否存在
  (display "1. 检查文件存在性:\n")
  (let ([file "/tmp/test-output.txt"])
    (display "  /tmp/test-output.txt 存在? ")
    (display (file-exists? file))
    (newline))
  
  ;; 获取文件信息
  (display "\n2. 文件信息:\n")
  (when (file-exists? "/tmp/test-output.txt")
    (display "  文件可读? ")
    (display (file-exists? "/tmp/test-output.txt"))
    (newline))
  
  (newline))

;; 5. 格式化输出
(define (demonstrate-formatted-output)
  (display "=== 格式化输出 ===\n\n")
  
  (display "1. 使用 format:\n")
  (display (format "  整数: ~a\n" 42))
  (display (format "  浮点: ~a\n" 3.14159))
  (display (format "  字符串: ~a\n" "Hello"))
  (display (format "  布尔: ~a\n" #t))
  
  (display "\n2. 使用 fprintf 写入文件:\n")
  (call-with-output-file "/tmp/formatted.txt"
    (lambda (port)
      (fprintf port "姓名: ~a\n" "张三")
      (fprintf port "年龄: ~a\n" 30)
      (fprintf port "分数: ~a\n" 95.5))
    'replace)
  (display "  已写入格式化数据到文件\n")
  
  ;; 读取并显示
  (display "\n3. 读取格式化文件:\n")
  (call-with-input-file "/tmp/formatted.txt"
    (lambda (port)
      (let loop ()
        (let ([line (get-line port)])
          (unless (eof-object? line)
            (display "  ") (display line) (newline)
            (loop))))))
  
  (newline))

;; 6. CSV 文件处理
(define (demonstrate-csv-processing)
  (display "=== CSV 文件处理 ===\n\n")
  
  ;; 创建 CSV 文件
  (display "1. 创建 CSV 文件:\n")
  (call-with-output-file "/tmp/data.csv"
    (lambda (port)
      (fprintf port "姓名,年龄,城市\n")
      (fprintf port "张三,25,北京\n")
      (fprintf port "李四,30,上海\n")
      (fprintf port "王五,28,广州\n"))
    'replace)
  (display "  已创建 CSV 文件\n")
  
  ;; 读取并解析 CSV
  (display "\n2. 解析 CSV 文件:\n")
  (call-with-input-file "/tmp/data.csv"
    (lambda (port)
      (let ([header (string-split (get-line port) #\,)])
        (display "  表头: ") (display header) (newline)
        (display "  数据:\n")
        (let loop ([row-num 1])
          (let ([line (get-line port)])
            (unless (eof-object? line)
              (let ([fields (string-split line #\,)])
                (display "    行 ") (display row-num) (display ": ")
                (display fields) (newline)
                (loop (+ row-num 1)))))))))
  
  (newline))

;; 7. 追加到文件
(define (demonstrate-append-mode)
  (display "=== 追加模式写入 ===\n\n")
  
  ;; 初始写入
  (call-with-output-file "/tmp/log.txt"
    (lambda (port)
      (fprintf port "日志开始\n"))
    'replace)
  
  ;; 追加写入
  (display "追加日志条目:\n")
  (do ([i 1 (+ i 1)])
      ((> i 5))
    (call-with-output-file "/tmp/log.txt"
      (lambda (port)
        (fprintf port "[~a] 日志条目 ~a\n" 
                (current-time) i))
      'append)
    (display "  添加条目 ") (display i) (newline))
  
  ;; 读取完整日志
  (display "\n完整日志内容:\n")
  (call-with-input-file "/tmp/log.txt"
    (lambda (port)
      (let loop ()
        (let ([line (get-line port)])
          (unless (eof-object? line)
            (display "  ") (display line) (newline)
            (loop))))))
  
  (newline))

;; 当前时间的简单表示
(define (current-time)
  (let ([t (current-date)])
    (format "~a-~a-~a ~a:~a:~a"
            (date-year t)
            (date-month t)
            (date-day t)
            (date-hour t)
            (date-minute t)
            (date-second t))))

;; 主函数
(define (main)
  (display "Chez Scheme 文件 I/O 示例\n")
  (display "==========================\n\n")
  
  (demonstrate-basic-io)
  (demonstrate-line-processing)
  (demonstrate-binary-io)
  (demonstrate-file-operations)
  (demonstrate-formatted-output)
  (demonstrate-csv-processing)
  (demonstrate-append-mode)
  
  (display "文件 I/O 示例完成！\n"))

;; 运行
(main)
