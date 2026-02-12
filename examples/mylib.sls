;; 简单的库示例
(library (mylib math)
  (export factorial fibonacci gcd-euclid prime?)
  (import (chezscheme))
  
  ;; 阶乘
  (define (factorial n)
    (let loop ([n n] [acc 1])
      (if (<= n 1)
          acc
          (loop (- n 1) (* n acc)))))
  
  ;; 斐波那契（优化版本）
  (define (fibonacci n)
    (let loop ([a 0] [b 1] [count n])
      (if (= count 0)
          a
          (loop b (+ a b) (- count 1)))))
  
  ;; 欧几里得算法求最大公约数
  (define (gcd-euclid a b)
    (if (= b 0)
        a
        (gcd-euclid b (remainder a b))))
  
  ;; 判断素数
  (define (prime? n)
    (cond
      [(<= n 1) #f]
      [(= n 2) #t]
      [(even? n) #f]
      [else
       (let loop ([i 3])
         (cond
           [(> (* i i) n) #t]
           [(= (remainder n i) 0) #f]
           [else (loop (+ i 2))]))]))
  
  ) ;; end of library

;; 字符串工具库
(library (mylib string-utils)
  (export string-split string-join string-trim string-reverse)
  (import (chezscheme))
  
  ;; 字符串分割
  (define (string-split str delimiter)
    (let ([len (string-length str)]
          [delim-len (string-length delimiter)])
      (let loop ([start 0] [result '()])
        (if (>= start len)
            (reverse result)
            (let ([pos (string-contains str delimiter start)])
              (if pos
                  (loop (+ pos delim-len)
                        (cons (substring str start pos) result))
                  (reverse (cons (substring str start len) result))))))))
  
  ;; 辅助函数：查找子串
  (define (string-contains str substr start)
    (let ([str-len (string-length str)]
          [sub-len (string-length substr)])
      (let loop ([i start])
        (cond
          [(> (+ i sub-len) str-len) #f]
          [(string=? (substring str i (+ i sub-len)) substr) i]
          [else (loop (+ i 1))]))))
  
  ;; 字符串连接
  (define (string-join strs delimiter)
    (if (null? strs)
        ""
        (let loop ([strs (cdr strs)] [result (car strs)])
          (if (null? strs)
              result
              (loop (cdr strs)
                    (string-append result delimiter (car strs)))))))
  
  ;; 去除首尾空白
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
  
  ;; 字符串反转
  (define (string-reverse str)
    (list->string (reverse (string->list str))))
  
  ) ;; end of library

;; 数据结构库：栈
(library (mylib stack)
  (export make-stack push! pop! peek empty? stack->list)
  (import (chezscheme))
  
  (define-record-type stack
    (fields (mutable items)))
  
  (define (make-stack)
    (make-stack '()))
  
  (define (push! stk item)
    (unless (stack? stk)
      (error 'push! "not a stack"))
    (stack-items-set! stk (cons item (stack-items stk))))
  
  (define (pop! stk)
    (unless (stack? stk)
      (error 'pop! "not a stack"))
    (when (null? (stack-items stk))
      (error 'pop! "stack is empty"))
    (let ([item (car (stack-items stk))])
      (stack-items-set! stk (cdr (stack-items stk)))
      item))
  
  (define (peek stk)
    (unless (stack? stk)
      (error 'peek "not a stack"))
    (when (null? (stack-items stk))
      (error 'peek "stack is empty"))
    (car (stack-items stk)))
  
  (define (empty? stk)
    (unless (stack? stk)
      (error 'empty? "not a stack"))
    (null? (stack-items stk)))
  
  (define (stack->list stk)
    (unless (stack? stk)
      (error 'stack->list "not a stack"))
    (stack-items stk))
  
  ) ;; end of library
