# Chez Scheme 快速参考

这是一份 Chez Scheme 的快速参考卡片，供有经验的程序员快速查阅。

## 基本语法

### 数据类型

```scheme
;; 数值
42                  ; 整数
1/3                 ; 有理数
3.14                ; 浮点数
3+4i                ; 复数

;; 字符和字符串
#\a                 ; 字符
"hello"             ; 字符串
"你好"              ; Unicode 字符串

;; 符号和标识符
'symbol             ; 符号
'hello-world        ; 多词符号

;; 列表和对
'(1 2 3)            ; 列表
(cons 1 2)          ; 点对
'()                 ; 空列表

;; 向量和字节向量
#(1 2 3)            ; 向量
#vu8(1 2 3)         ; 字节向量

;; 布尔值
#t                  ; 真
#f                  ; 假
```

### 变量定义

```scheme
(define x 10)                    ; 全局变量
(define (square x) (* x x))      ; 函数定义
(define add (lambda (x y) (+ x y)))  ; lambda 形式

(let ([x 1] [y 2])              ; 局部绑定
  (+ x y))

(let* ([x 1] [y (+ x 1)])       ; 顺序绑定
  (+ x y))

(letrec ([fact (lambda (n)       ; 递归绑定
                 (if (<= n 1)
                     1
                     (* n (fact (- n 1)))))])
  (fact 5))
```

### 控制流

```scheme
;; 条件
(if test then-expr else-expr)

(cond
  [test1 expr1 ...]
  [test2 expr2 ...]
  [else expr...])

(case expr
  [(val1) expr1]
  [(val2 val3) expr2]
  [else expr...])

(when test expr ...)
(unless test expr ...)

;; 循环
(do ([var init step] ...)
    (test result ...)
  body ...)

(let loop ([i 0])
  (when (< i 10)
    (display i)
    (loop (+ i 1))))
```

## 函数

### 定义函数

```scheme
;; 基本定义
(define (func arg1 arg2) body)

;; 可变参数
(define (func . args) body)           ; 所有参数为列表
(define (func arg1 arg2 . rest) body) ; 混合固定和可变

;; case-lambda（多重分派）
(define func
  (case-lambda
    [(x) (func x 0)]
    [(x y) (+ x y)]))
```

### Lambda 表达式

```scheme
(lambda (x) (* x x))                  ; 单参数
(lambda (x y) (+ x y))                ; 多参数
(lambda args (apply + args))          ; 可变参数
```

## 列表操作

```scheme
;; 构造
(cons x lst)          ; 添加到头部
(list x y z)          ; 创建列表
(append lst1 lst2)    ; 连接列表

;; 访问
(car lst)             ; 第一个元素
(cdr lst)             ; 剩余元素
(cadr lst)            ; (car (cdr lst))
(list-ref lst n)      ; 第 n 个元素

;; 测试
(null? lst)           ; 是否为空
(pair? obj)           ; 是否为对
(list? obj)           ; 是否为列表

;; 长度和反转
(length lst)          ; 列表长度
(reverse lst)         ; 反转列表

;; 高阶函数
(map proc lst1 lst2 ...)           ; 映射
(for-each proc lst1 lst2 ...)      ; 遍历
(filter pred lst)                  ; 过滤
(fold-left proc init lst)          ; 左折叠
(fold-right proc init lst)         ; 右折叠
```

## 字符串操作

```scheme
;; 构造和转换
(string char ...)                ; 从字符构造
(make-string n char)             ; 创建指定长度
(string->list str)               ; 转为字符列表
(list->string lst)               ; 从字符列表转换

;; 访问和修改
(string-ref str n)               ; 获取字符
(string-set! str n char)         ; 设置字符
(string-length str)              ; 字符串长度

;; 连接和提取
(string-append str1 str2 ...)    ; 连接字符串
(substring str start end)        ; 提取子串

;; 比较
(string=? str1 str2)             ; 相等
(string<? str1 str2)             ; 小于
(string-ci=? str1 str2)          ; 忽略大小写相等

;; 大小写转换
(string-upcase str)              ; 转大写
(string-downcase str)            ; 转小写
```

## 向量操作

```scheme
;; 构造
(vector x y z)                   ; 创建向量
(make-vector n init)             ; 创建指定长度

;; 访问和修改
(vector-ref vec n)               ; 获取元素
(vector-set! vec n val)          ; 设置元素
(vector-length vec)              ; 向量长度

;; 转换
(vector->list vec)               ; 转为列表
(list->vector lst)               ; 从列表转换

;; 遍历
(vector-for-each proc vec1 ...)  ; 遍历向量
(vector-map proc vec1 ...)       ; 映射向量
```

## 宏系统

### syntax-rules

```scheme
(define-syntax when
  (syntax-rules ()
    [(_ test body ...)
     (if test (begin body ...))]))

(define-syntax my-let
  (syntax-rules ()
    [(_ ([var val] ...) body ...)
     ((lambda (var ...) body ...) val ...)]))
```

### syntax-case

```scheme
(define-syntax swap!
  (lambda (x)
    (syntax-case x ()
      [(_ a b)
       #'(let ([tmp a])
           (set! a b)
           (set! b tmp))])))
```

## 记录类型

```scheme
;; 定义记录
(define-record-type person
  (fields name age)
  (protocol
   (lambda (new)
     (lambda (name age)
       (new name age)))))

;; 使用记录
(define p (make-person "Alice" 30))
(person? p)           ; => #t
(person-name p)       ; => "Alice"
(person-age p)        ; => 30
```

## I/O 操作

### 文件 I/O

```scheme
;; 读取文件
(call-with-input-file "file.txt"
  (lambda (port)
    (get-line port)))

;; 写入文件
(call-with-output-file "file.txt"
  (lambda (port)
    (display "Hello" port))
  'replace)

;; 追加到文件
(call-with-output-file "file.txt"
  (lambda (port)
    (display "World" port))
  'append)
```

### 端口操作

```scheme
(open-input-file filename)        ; 打开输入文件
(open-output-file filename)       ; 打开输出文件
(close-input-port port)           ; 关闭输入端口
(close-output-port port)          ; 关闭输出端口

(read port)                       ; 读取 S-表达式
(read-char port)                  ; 读取字符
(get-line port)                   ; 读取一行
(get-string-all port)             ; 读取全部内容

(write obj port)                  ; 写入对象
(display obj port)                ; 显示对象
(newline port)                    ; 写入换行
(fprintf port fmt args ...)       ; 格式化输出
```

## 库系统

### 定义库

```scheme
(library (mylib)
  (export func1 func2 var1)
  (import (chezscheme))
  
  (define (func1 x) ...)
  (define (func2 x y) ...)
  (define var1 42))
```

### 导入库

```scheme
(import (mylib))                        ; 导入全部
(import (only (mylib) func1))           ; 只导入 func1
(import (except (mylib) var1))          ; 排除 var1
(import (rename (mylib) (func1 f)))     ; 重命名
(import (prefix (mylib) ml:))           ; 添加前缀
```

## 性能优化

### Fixnum 操作

```scheme
;; Fixnum 算术（更快）
fx+  fx-  fx*  fx/                ; 算术运算
fx=?  fx<?  fx<=?  fx>?  fx>=?    ; 比较运算
fxand  fxior  fxxor               ; 位运算
fxsll  fxsrl                      ; 移位运算
```

### Flonum 操作

```scheme
;; Flonum 算术
fl+  fl-  fl*  fl/                ; 算术运算
fl=?  fl<?  fl<=?  fl>?  fl>=?    ; 比较运算
flsqrt  flexp  fllog              ; 数学函数
```

### 编译器优化

```scheme
(optimize-level 0)     ; 无优化
(optimize-level 3)     ; 最大优化

(define-integrable (square x) (* x x))  ; 内联函数
```

## 常用谓词

```scheme
;; 类型检查
number?  integer?  real?  complex?
char?  string?  symbol?
pair?  list?  null?
vector?  procedure?
boolean?  eof-object?

;; 数值谓词
zero?  positive?  negative?
even?  odd?
exact?  inexact?

;; 比较
eq?        ; 引用相等
eqv?       ; 值相等（更严格）
equal?     ; 结构相等

=  <  <=  >  >=                   ; 数值比较
char=?  char<?  string=?  string<?  ; 字符/字符串比较
```

## 异常处理

```scheme
(guard (ex
        [(condition1? ex) handle1]
        [(condition2? ex) handle2]
        [else default-handle])
  body ...)

(raise condition)                 ; 抛出异常
(error who message irritant ...)  ; 抛出错误
```

## Continuation

```scheme
(call/cc (lambda (k) ...))        ; 捕获 continuation
(call-with-current-continuation ...)  ; 同上（完整名）

;; 示例：提前返回
(define (find pred lst)
  (call/cc
   (lambda (return)
     (for-each
      (lambda (x)
        (when (pred x) (return x)))
      lst)
     #f)))
```

## 哈希表

```scheme
;; 创建哈希表
(make-eq-hashtable)               ; 使用 eq? 比较
(make-eqv-hashtable)              ; 使用 eqv? 比较
(make-hashtable hash-func equiv?) ; 自定义

;; 操作
(hashtable-set! ht key value)     ; 设置
(hashtable-ref ht key default)    ; 获取
(hashtable-delete! ht key)        ; 删除
(hashtable-contains? ht key)      ; 包含？
(hashtable-size ht)               ; 大小
(hashtable-keys ht)               ; 所有键
(hashtable-entries ht)            ; 键值对
```

## 实用函数

```scheme
;; 应用和组合
(apply proc arg ... lst)          ; 应用函数到列表
(compose f g)                     ; 函数组合 f ∘ g

;; 数值
(+ x ...)  (- x ...)  (* x ...)  (/ x ...)
(quotient n m)  (remainder n m)  (modulo n m)
(abs x)  (floor x)  (ceiling x)  (truncate x)
(sqrt x)  (expt x y)  (log x)  (exp x)
(sin x)  (cos x)  (tan x)

;; 类型转换
(number->string n)  (string->number s)
(char->integer c)  (integer->char n)
(symbol->string sym)  (string->symbol s)
```

## 调试工具

```scheme
(trace proc ...)                  ; 跟踪函数调用
(untrace proc ...)                ; 取消跟踪
(time expr)                       ; 测量执行时间
(debug-on-exception #t)           ; 异常时进入调试器
(break)                          ; 设置断点
```

## 常用模式

### 尾递归循环

```scheme
(define (sum-list lst)
  (let loop ([lst lst] [acc 0])
    (if (null? lst)
        acc
        (loop (cdr lst) (+ acc (car lst))))))
```

### 记忆化

```scheme
(define make-memo
  (lambda (f)
    (let ([table (make-eq-hashtable)])
      (lambda (x)
        (or (hashtable-ref table x #f)
            (let ([result (f x)])
              (hashtable-set! table x result)
              result))))))
```

### 管道操作

```scheme
(define (pipe . funcs)
  (lambda (x)
    (fold-left (lambda (acc f) (f acc)) x funcs)))

;; 使用
((pipe add1 square double) 5)  ; => ((5+1)^2)*2 = 72
```

---

## 参考资源

- **官方文档**: https://cisco.github.io/ChezScheme/
- **The Scheme Programming Language (4th Ed)**: https://www.scheme.com/tspl4/
- **R6RS 标准**: http://www.r6rs.org/

## 命令行选项

```bash
scheme                    # 启动 REPL
scheme --script file.ss   # 运行脚本
scheme --program file.ss  # 运行程序
scheme -q                 # 安静模式（不显示版本）
scheme --version          # 显示版本
scheme --libdirs "dir1:dir2"  # 设置库搜索路径
```

## 编译

```scheme
(compile-file "program.ss")           ; 编译为 .so
(load "program.so")                   ; 加载编译后的文件
```

---

此快速参考涵盖了 Chez Scheme 的核心功能。更多详细信息请参阅官方文档。
