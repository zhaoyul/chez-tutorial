;; 数学库
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
  )
