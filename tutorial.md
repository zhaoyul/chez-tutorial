# Chez Scheme 高级教程

本教程面向已经熟悉其他 Lisp 方言的高级程序员，深入介绍 Chez Scheme 的独特特性和高级功能。

## 目录

1. [简介](#简介)
2. [环境配置](#环境配置)
3. [核心语言特性](#核心语言特性)
4. [宏系统](#宏系统)
5. [模块和库系统](#模块和库系统)
6. [外部函数接口 (FFI)](#外部函数接口-ffi)
7. [性能优化](#性能优化)
8. [并发和并行](#并发和并行)
9. [内存管理](#内存管理)
10. [实战示例](#实战示例)
11. [调试和剖析](#调试和剖析)
12. [与其他 Scheme 实现的差异](#与其他-scheme-实现的差异)

---

## 简介

### Chez Scheme 的特点

Chez Scheme 是由 R. Kent Dybvig 开发的高性能 Scheme 实现，现已开源并被 Cisco 维护。主要特点：

- **极高性能**：原生代码编译器，接近 C 语言性能
- **完整的 R6RS 支持**：完全符合 R6RS 标准
- **增量编译**：支持 REPL 中的快速开发
- **强大的宏系统**：syntax-case 宏系统
- **成熟的 FFI**：与 C 语言的无缝集成
- **精确的垃圾回收**：世代垃圾回收器
- **丰富的运行时系统**：包括线程、I/O、图形等

### 与其他 Lisp 方言的主要区别

如果你熟悉 Common Lisp、Clojure 或其他 Scheme 实现：

- **宏系统**：使用 syntax-case 而非 defmacro（更卫生、更安全）
- **模块系统**：R6RS 库系统，编译时静态解析
- **命名约定**：使用 `-` 分隔符（如 `make-list`）而非 CL 的驼峰
- **尾调用优化**：严格保证尾递归优化
- **continuation**：一级 continuation 支持（call/cc）

---

## 环境配置

### 安装

```bash
# macOS
brew install chezscheme

# Ubuntu/Debian
sudo apt-get install chezscheme

# 从源码编译
git clone https://github.com/cisco/ChezScheme.git
cd ChezScheme
./configure
make
sudo make install
```

### REPL 使用

```scheme
$ scheme
Chez Scheme Version 9.5.8
Copyright 1984-2022 Cisco Systems, Inc.

> (+ 1 2 3)
6
```

### 编译和运行程序

```bash
# 编译为对象文件
echo '(compile-file "myprogram.ss")' | scheme -q

# 运行编译后的文件
scheme --script myprogram.so

# 直接运行脚本
scheme --script myprogram.ss
```

---

## 核心语言特性

### 数据类型

Chez Scheme 支持所有标准 Scheme 数据类型，并增加了扩展：

#### 数值

```scheme
;; 精确数值
42                  ; 整数
1/3                 ; 有理数
3+4i                ; 复数

;; 非精确数值
3.14                ; 浮点数
#e3.14              ; 转换为精确数 157/50
#i1/3               ; 转换为非精确数

;; 位操作
(fxior 5 3)         ; => 7  (按位或)
(fxand 5 3)         ; => 1  (按位与)
(fxsll 1 8)         ; => 256 (左移)
```

#### 字符串和字符

```scheme
;; 字符串
"Hello, 世界"        ; Unicode 字符串
#"raw string"       ; 原始字符串字面量

;; 字符
#\a                 ; 字符 a
#\space             ; 空格字符
#\x3b1              ; Unicode 字符 α

;; 字符串操作
(string-append "hello" " " "world")  ; => "hello world"
(substring "hello" 1 4)              ; => "ell"
(string-ref "hello" 0)               ; => #\h
```

#### 向量和字节向量

```scheme
;; 向量
#(1 2 3)                           ; 字面量向量
(make-vector 5 0)                  ; 创建长度为5，初始值为0的向量
(vector-ref #(1 2 3) 1)           ; => 2
(vector-set! v 0 'new-value)       ; 修改向量

;; 字节向量（高效的字节数组）
#vu8(1 2 3 4)                      ; 字节向量字面量
(make-bytevector 10 0)             ; 创建10字节的字节向量
```

#### 记录（Records）

Chez Scheme 的记录系统比简单的结构体更强大：

```scheme
;; 定义记录类型
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

;; 使用记录
(define p (make-person "Alice" 30))
(person? p)          ; => #t
(person-name p)      ; => "Alice"
(person-age p)       ; => 30

;; 继承
(define-record-type employee
  (parent person)
  (fields salary)
  (protocol
   (lambda (pargs->new)
     (lambda (name age salary)
       ((pargs->new name age) salary)))))

(define e (make-employee "Bob" 35 100000))
(employee-name e)    ; => "Bob"
(employee-salary e)  ; => 100000
```

### 控制流

#### 条件表达式

```scheme
;; if
(if (> x 0)
    'positive
    'non-positive)

;; cond
(cond
  [(< x 0) 'negative]
  [(= x 0) 'zero]
  [(> x 0) 'positive]
  [else 'unreachable])

;; case（类似 switch）
(case (car '(a b c))
  [(a) 'found-a]
  [(b c) 'found-b-or-c]
  [else 'not-found])

;; when 和 unless（单分支条件）
(when (file-exists? "config.txt")
  (load "config.txt"))

(unless (null? lst)
  (process-list lst))
```

#### 循环

```scheme
;; 尾递归（推荐方式）
(define (factorial n)
  (let loop ([n n] [acc 1])
    (if (<= n 1)
        acc
        (loop (- n 1) (* n acc)))))

;; named let（命名 let）
(let loop ([i 0] [sum 0])
  (if (>= i 10)
      sum
      (loop (+ i 1) (+ sum i))))

;; do 循环
(do ([i 0 (+ i 1)]
     [sum 0 (+ sum i)])
    ((>= i 10) sum))

;; for-each 和 map
(for-each display '(1 2 3 4 5))
(map (lambda (x) (* x x)) '(1 2 3 4 5))  ; => (1 4 9 16 25)
```

### 函数和闭包

```scheme
;; 基本函数定义
(define (square x) (* x x))

;; lambda 表达式
(lambda (x) (* x x))

;; 可变参数
(define (sum . args)
  (apply + args))

(sum 1 2 3 4 5)  ; => 15

;; 关键字参数（使用 case-lambda）
(define greet
  (case-lambda
    [(name) (format "Hello, ~a!" name)]
    [(title name) (format "Hello, ~a ~a!" title name)]))

(greet "Alice")          ; => "Hello, Alice!"
(greet "Dr." "Smith")    ; => "Hello, Dr. Smith!"

;; 闭包
(define (make-counter)
  (let ([count 0])
    (lambda ()
      (set! count (+ count 1))
      count)))

(define counter (make-counter))
(counter)  ; => 1
(counter)  ; => 2
(counter)  ; => 3
```

### Continuation

Chez Scheme 完全支持一级 continuation：

```scheme
;; call/cc 基础
(define (test-cc)
  (call/cc
   (lambda (k)
     (k 42)
     (display "This never prints")
     99)))

(test-cc)  ; => 42

;; 非局部退出
(define (find-first pred lst)
  (call/cc
   (lambda (return)
     (for-each
      (lambda (x)
        (when (pred x)
          (return x)))
      lst)
     #f)))

(find-first even? '(1 3 5 6 7 9))  ; => 6

;; 实现生成器
(define (make-generator proc)
  (let ([return #f])
    (lambda ()
      (call/cc
       (lambda (k)
         (set! return k)
         (proc (lambda (v)
                 (call/cc
                  (lambda (k)
                    (set! proc k)
                    (return v))))))))))

;; 使用生成器
(define gen
  (make-generator
   (lambda (yield)
     (let loop ([i 0])
       (yield i)
       (loop (+ i 1))))))

(gen)  ; => 0
(gen)  ; => 1
(gen)  ; => 2
```

---

## 宏系统

Chez Scheme 使用 syntax-case 宏系统，这是一个卫生宏系统，比传统的 defmacro 更安全、更强大。

### 基础宏定义

```scheme
;; 简单的宏
(define-syntax when
  (syntax-rules ()
    [(_ test body ...)
     (if test (begin body ...))]))

;; 使用
(when (> x 0)
  (display "positive")
  (newline))

;; 展开为：
;; (if (> x 0)
;;     (begin
;;       (display "positive")
;;       (newline)))
```

### syntax-rules

`syntax-rules` 是声明式的模式匹配宏系统：

```scheme
;; 基本模式匹配
(define-syntax my-let
  (syntax-rules ()
    [(_ ([var val] ...) body ...)
     ((lambda (var ...) body ...) val ...)]))

(my-let ([x 1] [y 2])
  (+ x y))  ; => 3

;; 递归宏
(define-syntax my-and
  (syntax-rules ()
    [(_) #t]
    [(_ test) test]
    [(_ test rest ...)
     (if test (my-and rest ...) #f)]))

;; 带字面量的宏
(define-syntax my-cond
  (syntax-rules (else)
    [(_ [else body ...])
     (begin body ...)]
    [(_ [test body ...] clause ...)
     (if test
         (begin body ...)
         (my-cond clause ...))]))
```

### syntax-case

`syntax-case` 提供了更强大的宏编程能力：

```scheme
;; 基本 syntax-case
(define-syntax swap!
  (lambda (x)
    (syntax-case x ()
      [(_ a b)
       #'(let ([tmp a])
           (set! a b)
           (set! b tmp))])))

;; 使用 with-syntax
(define-syntax define-getter-setter
  (lambda (x)
    (syntax-case x ()
      [(_ type field)
       (with-syntax ([getter (datum->syntax #'type 
                               (string->symbol
                                (format "~a-~a" 
                                  (syntax->datum #'type)
                                  (syntax->datum #'field))))]
                     [setter (datum->syntax #'type
                               (string->symbol
                                (format "set-~a-~a!"
                                  (syntax->datum #'type)
                                  (syntax->datum #'field))))])
         #'(begin
             (define getter ...)
             (define setter ...)))])))

;; 生成唯一标识符
(define-syntax with-gensyms
  (lambda (x)
    (syntax-case x ()
      [(_ (var ...) body ...)
       (with-syntax ([(temp ...) (generate-temporaries #'(var ...))])
         #'(let ([var (gensym)] ...)
             body ...))])))
```

### 高级宏技巧

#### 1. 模式变量和省略号

```scheme
;; 嵌套省略号
(define-syntax define-methods
  (syntax-rules ()
    [(_ type-name [method-name (arg ...) body ...] ...)
     (begin
       (define (method-name obj arg ...)
         (unless (type-name? obj)
           (error 'method-name "not a ~a" 'type-name))
         body ...)
       ...)]))

(define-methods person
  [greet () (format "Hello, I'm ~a" (person-name obj))]
  [age-in-years [years] (+ (person-age obj) years)])
```

#### 2. 语法参数（Syntax Parameters）

```scheme
(define-syntax-parameter current-value
  (lambda (stx)
    (syntax-error stx "used outside of with-value")))

(define-syntax with-value
  (syntax-rules ()
    [(_ val body ...)
     (syntax-parameterize ([current-value (lambda (stx) #'val)])
       body ...)]))

;; 使用
(with-value 42
  (display current-value))  ; 显示 42
```

#### 3. 宏展开时计算

```scheme
;; 在编译时计算值
(define-syntax compile-time-fib
  (lambda (x)
    (syntax-case x ()
      [(_ n)
       (let ([result (let fib ([n (syntax->datum #'n)])
                       (if (<= n 1)
                           n
                           (+ (fib (- n 1)) (fib (- n 2)))))])
         #`(quote #,result))])))

(compile-time-fib 10)  ; => 55，在编译时计算
```

#### 4. 创建 DSL

```scheme
;; 简单的模式匹配 DSL
(define-syntax match
  (syntax-rules (quote)
    [(_ val) (error 'match "no matching clause")]
    [(_ val ['quote pat] body ... rest ...)
     (if (equal? val 'pat)
         (begin body ...)
         (match val rest ...))]
    [(_ val pat body ... rest ...)
     (let ([pat val])
       (begin body ...))]
    [(_ val [pat guard body ...] rest ...)
     (let ([pat val])
       (if guard
           (begin body ...)
           (match val rest ...)))]))

;; 使用
(match x
  [0 (display "zero")]
  ['symbol (display "the symbol 'symbol")]
  [n (positive? n) (display "positive")]
  [n (display "negative or zero")])
```

---

## 模块和库系统

Chez Scheme 完全支持 R6RS 库系统，提供了模块化和命名空间管理。

### 库的基本结构

```scheme
;; 文件：mylib.sls
(library (mylib)
  (export make-point point-x point-y distance)
  (import (rnrs))
  
  (define-record-type point
    (fields x y))
  
  (define (distance p1 p2)
    (let ([dx (- (point-x p2) (point-x p1))]
          [dy (- (point-y p2) (point-y p1))])
      (sqrt (+ (* dx dx) (* dy dy))))))
```

### 导入库

```scheme
;; 导入整个库
(import (mylib))

;; 选择性导入
(import (only (mylib) make-point distance))

;; 排除某些导出
(import (except (mylib) distance))

;; 重命名
(import (rename (mylib) (distance point-distance)))

;; 添加前缀
(import (prefix (mylib) ml:))

;; 组合导入
(import (rename (only (mylib) make-point distance)
                (distance point-distance)))
```

### 库的版本控制

```scheme
;; 带版本的库
(library (mylib (1 0))
  (export ...)
  (import ...)
  ...)

;; 导入特定版本
(import (mylib (1 0)))

;; 版本范围
(import (mylib (>= 1)))
(import (mylib (and (>= 1) (< 2))))
```

### 分层库结构

```scheme
;; 文件：data/stack.sls
(library (data stack)
  (export make-stack push! pop! empty?)
  (import (rnrs))
  
  (define-record-type stack
    (fields (mutable items)))
  
  (define (make-stack)
    (make-stack '()))
  
  (define (push! stack item)
    (stack-items-set! stack (cons item (stack-items stack))))
  
  (define (pop! stack)
    (let ([items (stack-items stack)])
      (when (null? items)
        (error 'pop! "stack is empty"))
      (let ([item (car items)])
        (stack-items-set! stack (cdr items))
        item)))
  
  (define (empty? stack)
    (null? (stack-items stack))))

;; 使用
(import (data stack))
```

### 库的内部定义

```scheme
(library (mylib)
  (export public-func)
  (import (rnrs))
  
  ;; 这是私有的，不会导出
  (define (private-helper x)
    (* x x))
  
  ;; 这是公开的
  (define (public-func x)
    (+ (private-helper x) 1)))
```

### 条件展开

```scheme
(library (portable-lib)
  (export platform-specific-func)
  (import (rnrs))
  
  (define platform-specific-func
    (cond-expand
      [chezscheme
       (lambda () (display "Running on Chez Scheme"))]
      [guile
       (lambda () (display "Running on Guile"))]
      [else
       (lambda () (display "Unknown platform"))])))
```

---

## 外部函数接口 (FFI)

Chez Scheme 提供了强大的 FFI，允许直接调用 C 函数和操作 C 数据结构。

### 加载共享库

```scheme
;; 加载共享库
(load-shared-object "libm.so.6")    ; Linux
(load-shared-object "libm.dylib")   ; macOS
(load-shared-object "msvcrt.dll")   ; Windows

;; 或使用 #f 加载 C 运行时库
(load-shared-object #f)
```

### 声明外部函数

```scheme
;; 基本函数声明
(define strlen
  (foreign-procedure "strlen" (string) size_t))

(strlen "hello")  ; => 5

;; 带多个参数
(define strcmp
  (foreign-procedure "strcmp" (string string) int))

(strcmp "hello" "world")  ; => -1

;; 返回 void
(define printf
  (foreign-procedure "printf" (string) void))

;; 使用指针
(define malloc
  (foreign-procedure "malloc" (size_t) void*))

(define free
  (foreign-procedure "free" (void*) void))
```

### C 数据类型映射

```scheme
;; 基本类型
int          ; 整数
unsigned     ; 无符号整数
char         ; 字符
short        ; 短整数
long         ; 长整数
float        ; 单精度浮点
double       ; 双精度浮点
void         ; 无返回值
void*        ; 指针

;; 固定大小类型
int8         ; 8位整数
int16        ; 16位整数
int32        ; 32位整数
int64        ; 64位整数
unsigned-8   ; 8位无符号整数
unsigned-16  ; 16位无符号整数
unsigned-32  ; 32位无符号整数
unsigned-64  ; 64位无符号整数

;; Scheme 类型
scheme-object  ; 任意 Scheme 对象
```

### 使用外部指针

```scheme
;; 分配和释放内存
(define ptr (malloc 100))
(free ptr)

;; 读写指针
(define foreign-ref
  (lambda (ptr type offset)
    (case type
      [(int) (foreign-ref 'int ptr offset)]
      [(double) (foreign-ref 'double ptr offset)]
      [else (error 'foreign-ref "unsupported type")])))

(define foreign-set!
  (lambda (ptr type offset val)
    (case type
      [(int) (foreign-set! 'int ptr offset val)]
      [(double) (foreign-set! 'double ptr offset val)]
      [else (error 'foreign-set! "unsupported type")])))

;; 示例：操作整数数组
(let ([arr (malloc (* 10 (foreign-sizeof 'int)))])
  (do ([i 0 (+ i 1)])
      ((= i 10))
    (foreign-set! 'int arr (* i (foreign-sizeof 'int)) i))
  
  (do ([i 0 (+ i 1)])
      ((= i 10))
    (display (foreign-ref 'int arr (* i (foreign-sizeof 'int))))
    (newline))
  
  (free arr))
```

### 定义 C 结构体

```scheme
;; C 结构体：
;; struct point {
;;     double x;
;;     double y;
;; };

(define-ftype point
  (struct
    [x double]
    [y double]))

;; 分配结构体
(define p (make-ftype-pointer point (malloc (ftype-sizeof point))))

;; 访问字段
(ftype-set! point (x) p 3.14)
(ftype-set! point (y) p 2.71)

(ftype-ref point (x) p)  ; => 3.14
(ftype-ref point (y) p)  ; => 2.71

;; 释放
(free (ftype-pointer-address p))
```

### 回调函数

```scheme
;; 创建可以从 C 调用的 Scheme 函数
(define callback
  (foreign-callable
   (lambda (x)
     (display "Callback called with: ")
     (display x)
     (newline)
     (* x 2))
   (int) int))

;; 获取回调函数指针
(define callback-ptr (foreign-callable-entry-point callback))

;; 假设有一个 C 函数接受回调：
;; void apply_callback(int (*callback)(int), int value);
(define apply-callback
  (foreign-procedure "apply_callback" (void* int) void))

(apply-callback callback-ptr 42)
```

### 完整的 FFI 示例

```scheme
;; 封装 SQLite API
(library (sqlite)
  (export sqlite3-open sqlite3-close sqlite3-exec)
  (import (chezscheme))
  
  (load-shared-object "libsqlite3.so")
  
  ;; 类型定义
  (define-ftype sqlite3* void*)
  
  ;; 函数声明
  (define sqlite3-open-raw
    (foreign-procedure "sqlite3_open"
                       (string void*)
                       int))
  
  (define sqlite3-close-raw
    (foreign-procedure "sqlite3_close"
                       (void*)
                       int))
  
  ;; 包装函数
  (define (sqlite3-open filename)
    (let ([db-ptr (make-ftype-pointer sqlite3* (malloc (foreign-sizeof 'void*)))])
      (let ([rc (sqlite3-open-raw filename 
                                  (ftype-pointer-address db-ptr))])
        (if (= rc 0)
            (ftype-ref sqlite3* () db-ptr)
            (error 'sqlite3-open "failed to open database" rc)))))
  
  (define (sqlite3-close db)
    (let ([rc (sqlite3-close-raw db)])
      (unless (= rc 0)
        (error 'sqlite3-close "failed to close database" rc)))))
```

---

## 性能优化

### 编译器优化级别

```scheme
;; 在文件开头设置优化级别
(optimize-level 3)  ; 0 (无优化) 到 3 (最大优化)

;; 局部优化
(let ()
  (declare (optimize-level 3))
  (define (fast-function x)
    ...))
```

### 类型声明

虽然 Scheme 是动态类型语言，但 Chez 允许类型提示：

```scheme
;; 使用 fixnum 操作符（更快）
(define (fast-sum n)
  (let loop ([i 0] [sum 0])
    (if (fx>=? i n)
        sum
        (loop (fx+ i 1) (fx+ sum i)))))

;; fixnum 操作符
fx+  fx-  fx*  fx/       ; 算术
fx<?  fx<=?  fx>?  fx>=? ; 比较
fx=?  fx!=?              ; 相等
fxior  fxand  fxxor      ; 位运算

;; flonum 操作符
fl+  fl-  fl*  fl/       ; 浮点算术
fl<?  fl<=?  fl>?  fl>=? ; 浮点比较
```

### 避免装箱

```scheme
;; 慢：使用通用操作
(define (sum-list lst)
  (let loop ([lst lst] [sum 0])
    (if (null? lst)
        sum
        (loop (cdr lst) (+ sum (car lst))))))

;; 快：使用特定类型操作
(define (sum-fixnum-list lst)
  (let loop ([lst lst] [sum 0])
    (if (null? lst)
        sum
        (loop (cdr lst) (fx+ sum (car lst))))))
```

### 内联

```scheme
;; 使用 define-integrable 强制内联
(define-integrable (square x)
  (* x x))

;; 小函数自动内联
(define (use-square x)
  (square x))  ; 编译器会内联 square
```

### 向量 vs 列表

```scheme
;; 列表：O(n) 访问
(list-ref lst n)

;; 向量：O(1) 访问
(vector-ref vec n)

;; 对于频繁随机访问，使用向量
(define (sum-vector vec)
  (let ([len (vector-length vec)])
    (let loop ([i 0] [sum 0])
      (if (fx>=? i len)
          sum
          (loop (fx+ i 1) (fx+ sum (vector-ref vec i)))))))
```

### 避免不必要的分配

```scheme
;; 慢：每次迭代分配新 cons
(define (bad-range n)
  (let loop ([i 0] [acc '()])
    (if (>= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))

;; 快：预先分配向量
(define (good-range n)
  (let ([vec (make-vector n)])
    (do ([i 0 (+ i 1)])
        ((>= i n) vec)
      (vector-set! vec i i))))
```

### 使用局部定义

```scheme
;; 慢：每次查找全局绑定
(define (process-list lst)
  (map (lambda (x) (+ x 1)) lst))

;; 快：缓存到局部变量
(define (process-list lst)
  (let ([add1 (lambda (x) (+ x 1))])
    (map add1 lst)))
```

### 剖析和基准测试

```scheme
;; 时间测量
(time (factorial 1000))

;; 更详细的统计
(collect-request-handler)  ; 强制垃圾回收
(time (factorial 1000))

;; 统计信息
(statistics)  ; 显示内存和GC统计

;; 自定义基准
(define (benchmark proc n)
  (collect)
  (let ([start (current-time)])
    (do ([i 0 (+ i 1)])
        ((>= i n))
      (proc))
    (let ([end (current-time)])
      (- (time-second end) (time-second start)))))

;; 使用
(benchmark (lambda () (factorial 100)) 10000)
```

---

## 并发和并行

### 线程

Chez Scheme 支持真正的操作系统线程（需要启用线程支持编译）：

```scheme
;; 创建线程
(define t
  (fork-thread
   (lambda ()
     (display "Hello from thread!")
     (newline))))

;; 等待线程完成
(thread-join t)

;; 线程同步
(define mtx (make-mutex))
(define cv (make-condition))

;; 互斥锁使用
(define counter 0)
(define counter-mutex (make-mutex))

(define (increment-counter!)
  (with-mutex counter-mutex
    (set! counter (+ counter 1))))

;; 条件变量
(define queue '())
(define queue-mutex (make-mutex))
(define queue-cv (make-condition))

(define (enqueue! item)
  (with-mutex queue-mutex
    (set! queue (append queue (list item)))
    (condition-signal queue-cv)))

(define (dequeue!)
  (with-mutex queue-mutex
    (let loop ()
      (if (null? queue)
          (begin
            (condition-wait queue-cv queue-mutex)
            (loop))
          (let ([item (car queue)])
            (set! queue (cdr queue))
            item)))))
```

### Future 和 Promise

```scheme
;; 创建 future（延迟计算）
(define f
  (make-thread-parameter 'not-ready))

(define (compute-future)
  (fork-thread
   (lambda ()
     (let ([result (expensive-computation)])
       (f result)))))

;; 获取 future 的值
(define (future-get f)
  (thread-join f)
  (f))
```

### 并行映射

```scheme
;; 并行 map
(define (parallel-map proc lst)
  (let ([threads
         (map (lambda (x)
                (fork-thread (lambda () (proc x))))
              lst)])
    (map thread-join threads)))

;; 使用
(parallel-map (lambda (x) (heavy-computation x)) '(1 2 3 4 5))
```

---

## 内存管理

### 垃圾回收

```scheme
;; 手动触发 GC
(collect)

;; 获取 GC 统计
(collect-notify #t)  ; 启用 GC 通知

;; 设置内存限制
(bytes-allocated)    ; 当前分配的字节数
(collect-trip-bytes) ; GC 触发阈值
```

### 弱引用

```scheme
;; 弱对（weak pairs）
(define wp (weak-cons key value))
(car wp)  ; => key (可能是 #f 如果被 GC)
(cdr wp)  ; => value

;; 弱哈希表
(define wht (make-weak-eq-hashtable))
(hashtable-set! wht key value)
(hashtable-ref wht key #f)  ; 可能返回 #f 如果 key 被 GC
```

### 守护者（Guardians）

```scheme
;; 创建守护者
(define g (make-guardian))

;; 注册对象
(define obj (make-some-object))
(g obj)

;; 当 obj 即将被 GC 时，可以从守护者中取出
(let ([rescued (g)])
  (when rescued
    (finalize-object rescued)))
```

---

## 实战示例

### 示例 1: 简单的 Web 服务器

```scheme
(library (simple-http-server)
  (export start-server)
  (import (chezscheme))
  
  (define (start-server port)
    (let ([server-socket (create-server-socket port)])
      (let loop ()
        (let-values ([(client-socket client-addr) 
                      (accept-socket server-socket)])
          (fork-thread
           (lambda ()
             (handle-client client-socket)))
          (loop)))))
  
  (define (handle-client socket)
    (let ([request (read-http-request socket)])
      (let ([response (generate-response request)])
        (write-http-response socket response))
      (close-socket socket)))
  
  (define (read-http-request socket)
    ;; 简化的 HTTP 请求解析
    (let ([line (read-line (socket-input-port socket))])
      (parse-http-request-line line)))
  
  (define (generate-response request)
    (case (car request)
      [(GET)
       '(200 "OK" "text/html" "<html><body><h1>Hello, World!</h1></body></html>")]
      [else
       '(404 "Not Found" "text/plain" "Not found")]))
  
  (define (write-http-response socket response)
    (let ([out (socket-output-port socket)])
      (fprintf out "HTTP/1.1 ~a ~a\r\n" (car response) (cadr response))
      (fprintf out "Content-Type: ~a\r\n" (caddr response))
      (fprintf out "Content-Length: ~a\r\n" 
               (string-length (cadddr response)))
      (fprintf out "\r\n")
      (fprintf out "~a" (cadddr response))
      (flush-output-port out))))
```

### 示例 2: JSON 解析器

```scheme
(library (json)
  (export json-parse json-stringify)
  (import (chezscheme))
  
  (define (json-parse str)
    (let ([p (open-string-input-port str)])
      (parse-value p)))
  
  (define (parse-value p)
    (skip-whitespace p)
    (let ([c (peek-char p)])
      (cond
        [(eof-object? c) (error 'json-parse "unexpected EOF")]
        [(char=? c #\{) (parse-object p)]
        [(char=? c #\[) (parse-array p)]
        [(char=? c #\") (parse-string p)]
        [(or (char-numeric? c) (char=? c #\-)) (parse-number p)]
        [(char=? c #\t) (parse-true p)]
        [(char=? c #\f) (parse-false p)]
        [(char=? c #\n) (parse-null p)]
        [else (error 'json-parse "unexpected character" c)])))
  
  (define (parse-object p)
    (read-char p)  ; 读取 '{'
    (skip-whitespace p)
    (if (char=? (peek-char p) #\})
        (begin (read-char p) '())
        (let loop ([result '()])
          (skip-whitespace p)
          (let ([key (parse-string p)])
            (skip-whitespace p)
            (unless (char=? (read-char p) #\:)
              (error 'json-parse "expected ':'"))
            (let ([value (parse-value p)])
              (skip-whitespace p)
              (let ([c (read-char p)])
                (cond
                  [(char=? c #\}) (reverse (cons (cons key value) result))]
                  [(char=? c #\,) (loop (cons (cons key value) result))]
                  [else (error 'json-parse "expected ',' or '}'" c)])))))))
  
  (define (parse-array p)
    (read-char p)  ; 读取 '['
    (skip-whitespace p)
    (if (char=? (peek-char p) #\])
        (begin (read-char p) '())
        (let loop ([result '()])
          (let ([value (parse-value p)])
            (skip-whitespace p)
            (let ([c (read-char p)])
              (cond
                [(char=? c #\]) (reverse (cons value result))]
                [(char=? c #\,) (loop (cons value result))]
                [else (error 'json-parse "expected ',' or ']'" c)]))))))
  
  (define (parse-string p)
    (read-char p)  ; 读取 '"'
    (let loop ([chars '()])
      (let ([c (read-char p)])
        (cond
          [(eof-object? c) (error 'json-parse "unexpected EOF in string")]
          [(char=? c #\") (list->string (reverse chars))]
          [(char=? c #\\)
           (let ([next (read-char p)])
             (loop (cons (unescape-char next) chars)))]
          [else (loop (cons c chars))]))))
  
  (define (parse-number p)
    (let loop ([chars '()])
      (let ([c (peek-char p)])
        (if (or (char-numeric? c) 
                (char=? c #\.) 
                (char=? c #\e) 
                (char=? c #\E)
                (char=? c #\+)
                (char=? c #\-))
            (begin
              (read-char p)
              (loop (cons c chars)))
            (string->number (list->string (reverse chars)))))))
  
  (define (skip-whitespace p)
    (let loop ()
      (let ([c (peek-char p)])
        (when (and (char? c) (char-whitespace? c))
          (read-char p)
          (loop)))))
  
  (define (json-stringify obj)
    (call-with-string-output-port
     (lambda (p)
       (write-json-value obj p))))
  
  (define (write-json-value obj p)
    (cond
      [(null? obj) (display "null" p)]
      [(boolean? obj) (display (if obj "true" "false") p)]
      [(number? obj) (display obj p)]
      [(string? obj) (write-json-string obj p)]
      [(pair? obj)
       (if (string? (car obj))
           (write-json-object obj p)
           (write-json-array obj p))]
      [else (error 'json-stringify "unsupported type" obj)]))
  
  (define (write-json-string str p)
    (display "\"" p)
    (string-for-each
     (lambda (c)
       (case c
         [(#\") (display "\\\"" p)]
         [(#\\) (display "\\\\" p)]
         [(#\newline) (display "\\n" p)]
         [(#\return) (display "\\r" p)]
         [(#\tab) (display "\\t" p)]
         [else (display c p)]))
     str)
    (display "\"" p))
  
  (define (write-json-object obj p)
    (display "{" p)
    (let loop ([obj obj] [first? #t])
      (unless (null? obj)
        (unless first? (display "," p))
        (write-json-string (caar obj) p)
        (display ":" p)
        (write-json-value (cdar obj) p)
        (loop (cdr obj) #f)))
    (display "}" p))
  
  (define (write-json-array arr p)
    (display "[" p)
    (let loop ([arr arr] [first? #t])
      (unless (null? arr)
        (unless first? (display "," p))
        (write-json-value (car arr) p)
        (loop (cdr arr) #f)))
    (display "]" p)))
```

### 示例 3: 简单的测试框架

```scheme
(library (test-framework)
  (export define-test-suite test assert-equal assert-true 
          assert-false run-tests)
  (import (chezscheme))
  
  (define *test-suites* '())
  
  (define-syntax define-test-suite
    (syntax-rules ()
      [(_ name test ...)
       (set! *test-suites*
         (cons (list 'name (list test ...))
               *test-suites*))]))
  
  (define-syntax test
    (syntax-rules ()
      [(_ name body ...)
       (lambda ()
         (display "  Testing: ")
         (display 'name)
         (display " ... ")
         (guard (ex
                 [else
                  (display "FAILED\n")
                  (display "    Error: ")
                  (display (condition-message ex))
                  (newline)
                  #f])
           (begin body ...)
           (display "OK\n")
           #t))]))
  
  (define (assert-equal expected actual)
    (unless (equal? expected actual)
      (error 'assert-equal
             (format "expected ~s but got ~s" expected actual))))
  
  (define (assert-true value)
    (unless value
      (error 'assert-true "expected true value")))
  
  (define (assert-false value)
    (when value
      (error 'assert-false "expected false value")))
  
  (define (run-tests)
    (display "Running tests...\n")
    (let ([total 0]
          [passed 0])
      (for-each
       (lambda (suite)
         (let ([name (car suite)]
               [tests (cadr suite)])
           (display "Suite: ")
           (display name)
           (newline)
           (for-each
            (lambda (test)
              (set! total (+ total 1))
              (when (test)
                (set! passed (+ passed 1))))
            tests)))
       (reverse *test-suites*))
      (newline)
      (display "Results: ")
      (display passed)
      (display "/")
      (display total)
      (display " tests passed\n")
      (= passed total))))
```

---

## 调试和剖析

### 调试器

```scheme
;; 启用调试模式
(debug-on-exception #t)

;; 断点
(break)  ; 进入调试器

;; 跟踪函数调用
(trace factorial)
(factorial 5)
(untrace factorial)

;; 自定义跟踪
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

(debug (+ 1 2 3))  ; 输出: DEBUG: (+ 1 2 3) => 6
```

### 错误处理

```scheme
;; guard 表达式（类似 try-catch）
(guard (ex
        [(file-error? ex)
         (display "File error occurred")
         #f]
        [else
         (display "Other error: ")
         (display (condition-message ex))
         #f])
  (open-input-file "nonexistent.txt"))

;; 自定义异常
(define-condition-type &custom-error &error
  make-custom-error custom-error?)

(define (raise-custom-error msg)
  (raise (condition
          (make-custom-error)
          (make-message-condition msg))))

;; 使用
(guard (ex
        [(custom-error? ex)
         (display "Custom error caught!")])
  (raise-custom-error "Something went wrong"))
```

### 性能剖析

```scheme
;; 基本时间测量
(time (fibonacci 30))

;; 详细统计
(collect)  ; 先进行垃圾回收
(time
  (begin
    (fibonacci 30)
    (fibonacci 30)
    (fibonacci 30)))

;; 获取统计信息
(statistics)

;; 自定义剖析器
(define (profile proc)
  (let ([start-time (current-time)]
        [start-bytes (bytes-allocated)])
    (let ([result (proc)])
      (let ([end-time (current-time)]
            [end-bytes (bytes-allocated)])
        (display "Time: ")
        (display (time-difference end-time start-time))
        (newline)
        (display "Memory: ")
        (display (- end-bytes start-bytes))
        (display " bytes")
        (newline)
        result))))
```

---

## 与其他 Scheme 实现的差异

### 与 Racket 的差异

```scheme
;; Racket 使用 #lang
#lang racket

;; Chez 使用 library
(library (mylib) ...)

;; Racket 的 struct
(struct point (x y))

;; Chez 的 record
(define-record-type point
  (fields x y))

;; Racket 的 for 循环
(for ([i (in-range 10)])
  (display i))

;; Chez 的 do 循环
(do ([i 0 (+ i 1)])
    ((>= i 10))
  (display i))
```

### 与 Guile 的差异

```scheme
;; Guile 模块
(define-module (mymodule)
  #:export (my-func))

;; Chez 库
(library (mymodule)
  (export my-func)
  (import (rnrs))
  ...)

;; Guile 的参数对象
(define my-param (make-parameter 42))

;; Chez 也有参数对象，语法相同
(define my-param (make-parameter 42))
```

### 与 Chicken Scheme 的差异

```scheme
;; Chicken 的单元系统
(unit myunit ...)

;; Chez 的库系统
(library (myunit) ...)

;; Chicken 的 FFI
(foreign-lambda* int ((int x)) "return x * 2;")

;; Chez 的 FFI
(define double-it
  (foreign-procedure "double_it" (int) int))
```

---

## 最佳实践

### 1. 代码组织

```scheme
;; 使用库组织代码
(library (myapp core)
  (export ...)
  (import ...)
  
  ;; 核心功能
  ...)

(library (myapp utils)
  (export ...)
  (import ...)
  
  ;; 工具函数
  ...)

(library (myapp)
  (export ...)
  (import (myapp core)
          (myapp utils))
  
  ;; 公共 API
  ...)
```

### 2. 错误处理

```scheme
;; 总是提供有用的错误信息
(define (divide x y)
  (when (zero? y)
    (error 'divide "division by zero" x y))
  (/ x y))

;; 使用 guard 处理异常
(define (safe-divide x y)
  (guard (ex
          [(and (error? ex)
                (equal? (condition-who ex) 'divide))
           (display "Cannot divide by zero")
           #f])
    (divide x y)))
```

### 3. 性能考虑

```scheme
;; 使用适当的数据结构
;; 列表：顺序访问
;; 向量：随机访问
;; 哈希表：键值查找

;; 使用类型特定操作
(fx+ a b)    ; 而不是 (+ a b) 对于 fixnum
(fl+ a b)    ; 而不是 (+ a b) 对于 flonum

;; 避免不必要的装箱
(define (sum-vector v)
  (let ([len (vector-length v)])
    (let loop ([i 0] [sum 0.0])
      (if (fx>=? i len)
          sum
          (loop (fx+ i 1) (fl+ sum (vector-ref v i)))))))
```

### 4. 可读性

```scheme
;; 使用有意义的命名
(define (calculate-total-price items tax-rate)
  ...)

;; 而不是
(define (calc items tr)
  ...)

;; 适当使用注释
;; 计算包含税费的总价
;; items: 商品列表，每个商品是 (name . price) 对
;; tax-rate: 税率，0.0 到 1.0 之间
;; 返回: 总价（浮点数）
(define (calculate-total-price items tax-rate)
  ...)
```

### 5. 测试

```scheme
;; 为关键功能编写测试
(define-test-suite math-tests
  (test "addition"
    (assert-equal 4 (+ 2 2)))
  
  (test "division"
    (assert-equal 2 (/ 4 2)))
  
  (test "division by zero"
    (assert-error (/ 1 0))))
```

---

## 进阶主题

### 延续传递风格 (CPS)

```scheme
;; 直接风格
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;; CPS 风格
(define (factorial-cps n k)
  (if (<= n 1)
      (k 1)
      (factorial-cps (- n 1)
                     (lambda (v)
                       (k (* n v))))))

(factorial-cps 5 (lambda (x) x))  ; => 120
```

### 元编程

```scheme
;; 动态代码生成
(define (make-adder n)
  (eval `(lambda (x) (+ x ,n)) (environment '(rnrs))))

((make-adder 5) 10)  ; => 15

;; 宏生成宏
(define-syntax define-binary-op
  (syntax-rules ()
    [(_ name op)
     (define-syntax name
       (syntax-rules ()
         [(_ a b) (op a b)]))]))

(define-binary-op my-add +)
(my-add 1 2)  ; => 3
```

### 惰性求值

```scheme
;; 延迟求值
(define-syntax delay
  (syntax-rules ()
    [(_ expr) (lambda () expr)]))

(define-syntax force
  (syntax-rules ()
    [(_ promise) (promise)]))

;; 无限流
(define (integers-from n)
  (cons n (delay (integers-from (+ n 1)))))

(define (stream-take n stream)
  (if (= n 0)
      '()
      (cons (car stream)
            (stream-take (- n 1) (force (cdr stream))))))

(stream-take 10 (integers-from 1))
;; => (1 2 3 4 5 6 7 8 9 10)
```

---

## 资源和参考

### 官方文档

- **The Scheme Programming Language (4th Edition)** by R. Kent Dybvig
  - 最权威的 Scheme 语言书籍
  - 包含大量示例和最佳实践

- **The Chez Scheme User's Guide**
  - Chez Scheme 特定功能的详细文档
  - 包含 FFI、性能优化等高级主题

- **R6RS Scheme 标准**
  - Scheme 语言的正式规范
  - Chez Scheme 完全兼容

### 在线资源

- Chez Scheme 官方网站：https://cisco.github.io/ChezScheme/
- GitHub 仓库：https://github.com/cisco/ChezScheme
- Scheme Wiki：http://community.schemewiki.org/

### 推荐阅读

1. **Structure and Interpretation of Computer Programs (SICP)**
   - 经典的计算机科学教材
   - 使用 Scheme 讲解编程概念

2. **The Little Schemer** 系列
   - 通过问答形式学习 Scheme
   - 适合初学者

3. **Essentials of Programming Languages (EOPL)**
   - 深入讲解编程语言实现
   - 使用 Scheme 实现解释器

---

## 总结

Chez Scheme 是一个强大、高性能的 Scheme 实现，特别适合：

1. **需要高性能的应用**：原生代码编译，接近 C 语言性能
2. **大型项目**：完善的模块系统和工具链
3. **系统编程**：强大的 FFI 支持
4. **语言研究**：完整的 continuation 支持和强大的宏系统

作为已经熟悉其他 Lisp 的高级程序员，你会发现 Chez Scheme 的优势在于：

- 更严格的语义和尾调用优化保证
- 更卫生和安全的宏系统
- 更高的性能
- 更现代的模块系统

希望这份教程能帮助你快速掌握 Chez Scheme 的核心概念和高级特性！

---

## 练习建议

1. **实现一个小型解释器**：使用 Chez Scheme 实现一个简单语言的解释器
2. **FFI 项目**：使用 FFI 封装一个 C 库（如 SQLite、SDL 等）
3. **Web 应用**：构建一个完整的 Web 应用，包括服务器和模板系统
4. **DSL 设计**：设计一个领域特定语言，充分利用宏系统
5. **性能优化**：选择一个算法，比较不同实现方式的性能差异

祝学习愉快！
