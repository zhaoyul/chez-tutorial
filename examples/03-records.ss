#!/usr/bin/env scheme-script
;; 记录类型示例

;; 1. 基本记录定义
(define-record-type person
  (fields name age)
  (protocol
   (lambda (new)
     (lambda (name age)
       (unless (string? name)
         (error 'make-person "name must be a string"))
       (unless (and (integer? age) (>= age 0))
         (error 'make-person "age must be non-negative integer"))
       (new name age)))))

;; 2. 带有可变字段的记录
(define-record-type bank-account
  (fields number (mutable balance))
  (protocol
   (lambda (new)
     (lambda (number initial-balance)
       (new number initial-balance)))))

;; 银行账户操作
(define (deposit! account amount)
  (unless (bank-account? account)
    (error 'deposit! "not a bank account"))
  (unless (and (number? amount) (positive? amount))
    (error 'deposit! "amount must be positive"))
  (bank-account-balance-set! account
    (+ (bank-account-balance account) amount)))

(define (withdraw! account amount)
  (unless (bank-account? account)
    (error 'withdraw! "not a bank account"))
  (unless (and (number? amount) (positive? amount))
    (error 'withdraw! "amount must be positive"))
  (when (< (bank-account-balance account) amount)
    (error 'withdraw! "insufficient funds"))
  (bank-account-balance-set! account
    (- (bank-account-balance account) amount)))

;; 3. 记录继承
(define-record-type employee
  (parent person)
  (fields employee-id salary)
  (protocol
   (lambda (pargs->new)
     (lambda (name age employee-id salary)
       ((pargs->new name age) employee-id salary)))))

;; 4. 更复杂的记录：树节点
(define-record-type tree-node
  (fields value (mutable left) (mutable right))
  (protocol
   (lambda (new)
     (case-lambda
       [(value) (new value #f #f)]
       [(value left right) (new value left right)]))))

;; 树操作
(define (insert-bst tree value)
  (cond
    [(not tree) (make-tree-node value)]
    [(< value (tree-node-value tree))
     (tree-node-left-set! tree (insert-bst (tree-node-left tree) value))
     tree]
    [(> value (tree-node-value tree))
     (tree-node-right-set! tree (insert-bst (tree-node-right tree) value))
     tree]
    [else tree]))

(define (inorder-traversal tree)
  (when tree
    (inorder-traversal (tree-node-left tree))
    (display (tree-node-value tree))
    (display " ")
    (inorder-traversal (tree-node-right tree))))

;; 5. 泛型记录：点和向量
(define-record-type point
  (fields x y)
  (protocol
   (lambda (new)
     (lambda (x y)
       (unless (and (real? x) (real? y))
         (error 'make-point "coordinates must be real numbers"))
       (new x y)))))

(define (distance p1 p2)
  (let ([dx (- (point-x p2) (point-x p1))]
        [dy (- (point-y p2) (point-y p1))])
    (sqrt (+ (* dx dx) (* dy dy)))))

(define (point-add p1 p2)
  (make-point (+ (point-x p1) (point-x p2))
              (+ (point-y p1) (point-y p2))))

;; 测试函数
(define (test-records)
  (display "=== 记录类型示例 ===\n\n")
  
  ;; 测试基本记录
  (display "1. 基本记录（Person）:\n")
  (define p1 (make-person "Alice" 30))
  (display "  姓名: ") (display (person-name p1)) (newline)
  (display "  年龄: ") (display (person-age p1)) (newline)
  (display "  是 person? ") (display (person? p1)) (newline)
  
  ;; 测试银行账户
  (display "\n2. 可变字段记录（Bank Account）:\n")
  (define acc (make-bank-account "123456" 1000))
  (display "  账号: ") (display (bank-account-number acc)) (newline)
  (display "  初始余额: ") (display (bank-account-balance acc)) (newline)
  (deposit! acc 500)
  (display "  存入 500 后: ") (display (bank-account-balance acc)) (newline)
  (withdraw! acc 200)
  (display "  取出 200 后: ") (display (bank-account-balance acc)) (newline)
  
  ;; 测试继承
  (display "\n3. 记录继承（Employee）:\n")
  (define e1 (make-employee "Bob" 35 "E001" 75000))
  (display "  姓名: ") (display (employee-name e1)) (newline)
  (display "  年龄: ") (display (employee-age e1)) (newline)
  (display "  员工号: ") (display (employee-employee-id e1)) (newline)
  (display "  薪水: ") (display (employee-salary e1)) (newline)
  (display "  是 person? ") (display (person? e1)) (newline)
  (display "  是 employee? ") (display (employee? e1)) (newline)
  
  ;; 测试树
  (display "\n4. 二叉搜索树:\n")
  (define tree #f)
  (set! tree (insert-bst tree 5))
  (set! tree (insert-bst tree 3))
  (set! tree (insert-bst tree 7))
  (set! tree (insert-bst tree 1))
  (set! tree (insert-bst tree 9))
  (display "  中序遍历: ")
  (inorder-traversal tree)
  (newline)
  
  ;; 测试点
  (display "\n5. 点和距离:\n")
  (define pt1 (make-point 0 0))
  (define pt2 (make-point 3 4))
  (display "  点1: (") 
  (display (point-x pt1)) (display ", ")
  (display (point-y pt1)) (display ")\n")
  (display "  点2: (")
  (display (point-x pt2)) (display ", ")
  (display (point-y pt2)) (display ")\n")
  (display "  距离: ") (display (distance pt1 pt2)) (newline)
  (define pt3 (point-add pt1 pt2))
  (display "  相加: (")
  (display (point-x pt3)) (display ", ")
  (display (point-y pt3)) (display ")\n")
  
  (newline))

;; 运行测试
(test-records)

;; 6. 实际应用：简单的图书管理系统
(define-record-type book
  (fields title author isbn (mutable available))
  (protocol
   (lambda (new)
     (lambda (title author isbn)
       (new title author isbn #t)))))

(define-record-type library
  (fields name (mutable books))
  (protocol
   (lambda (new)
     (lambda (name)
       (new name '())))))

(define (add-book! library book)
  (library-books-set! library
    (cons book (library-books library))))

(define (find-book library isbn)
  (let loop ([books (library-books library)])
    (cond
      [(null? books) #f]
      [(string=? (book-isbn (car books)) isbn) (car books)]
      [else (loop (cdr books))])))

(define (checkout-book! library isbn)
  (let ([book (find-book library isbn)])
    (cond
      [(not book) (display "书籍不存在\n")]
      [(not (book-available book)) (display "书籍已被借出\n")]
      [else
       (book-available-set! book #f)
       (display "借书成功\n")])))

(define (return-book! library isbn)
  (let ([book (find-book library isbn)])
    (cond
      [(not book) (display "书籍不存在\n")]
      [(book-available book) (display "书籍未被借出\n")]
      [else
       (book-available-set! book #t)
       (display "还书成功\n")])))

(define (test-library-system)
  (display "=== 图书管理系统示例 ===\n\n")
  
  (define lib (make-library "市图书馆"))
  
  (add-book! lib (make-book "SICP" "Abelson & Sussman" "978-0262510871"))
  (add-book! lib (make-book "The Little Schemer" "Friedman & Felleisen" "978-0262560993"))
  
  (display "图书馆: ") (display (library-name lib)) (newline)
  (display "藏书数量: ") (display (length (library-books lib))) (newline)
  
  (display "\n借书: SICP\n")
  (checkout-book! lib "978-0262510871")
  
  (display "\n再次借书: SICP\n")
  (checkout-book! lib "978-0262510871")
  
  (display "\n还书: SICP\n")
  (return-book! lib "978-0262510871")
  
  (newline))

(test-library-system)
