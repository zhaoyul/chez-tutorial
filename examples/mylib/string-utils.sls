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
