# Chez Scheme vs 其他语言对比

本文档帮助来自其他编程语言背景的开发者快速理解 Chez Scheme 的语法和概念。

## Python vs Chez Scheme

### 基本语法

```python
# Python
x = 10
y = 20
result = x + y

def square(x):
    return x * x

numbers = [1, 2, 3, 4, 5]
squares = [x**2 for x in numbers]
```

```scheme
;; Chez Scheme
(define x 10)
(define y 20)
(define result (+ x y))

(define (square x)
  (* x x))

(define numbers '(1 2 3 4 5))
(define squares (map (lambda (x) (* x x)) numbers))
```

### 控制流

```python
# Python
if x > 0:
    print("positive")
elif x < 0:
    print("negative")
else:
    print("zero")

for i in range(10):
    print(i)

while condition:
    # do something
    pass
```

```scheme
;; Chez Scheme
(cond
  [(> x 0) (display "positive")]
  [(< x 0) (display "negative")]
  [else (display "zero")])

(do ([i 0 (+ i 1)])
    ((>= i 10))
  (display i))

(let loop ()
  (when condition
    ;; do something
    (loop)))
```

### 函数式编程

```python
# Python
from functools import reduce

# Map
result = list(map(lambda x: x * 2, [1, 2, 3]))

# Filter
result = list(filter(lambda x: x > 0, [-1, 0, 1, 2]))

# Reduce
result = reduce(lambda x, y: x + y, [1, 2, 3, 4], 0)
```

```scheme
;; Chez Scheme
;; Map
(define result (map (lambda (x) (* x 2)) '(1 2 3)))

;; Filter
(define result (filter (lambda (x) (> x 0)) '(-1 0 1 2)))

;; Reduce (fold)
(define result (fold-left + 0 '(1 2 3 4)))
```

---

## JavaScript vs Chez Scheme

### 基本语法

```javascript
// JavaScript
const x = 10;
let y = 20;
const result = x + y;

function square(x) {
    return x * x;
}

const numbers = [1, 2, 3, 4, 5];
const squares = numbers.map(x => x ** 2);
```

```scheme
;; Chez Scheme
(define x 10)
(define y 20)
(define result (+ x y))

(define (square x)
  (* x x))

(define numbers '(1 2 3 4 5))
(define squares (map (lambda (x) (* x x)) numbers))
```

### 闭包

```javascript
// JavaScript
function makeCounter() {
    let count = 0;
    return function() {
        count++;
        return count;
    };
}

const counter = makeCounter();
console.log(counter()); // 1
console.log(counter()); // 2
```

```scheme
;; Chez Scheme
(define (make-counter)
  (let ([count 0])
    (lambda ()
      (set! count (+ count 1))
      count)))

(define counter (make-counter))
(counter)  ; => 1
(counter)  ; => 2
```

### Promise/异步 vs Continuation

```javascript
// JavaScript - Promise
function fetchData() {
    return new Promise((resolve, reject) => {
        // async operation
        resolve(data);
    });
}

fetchData()
    .then(data => process(data))
    .catch(error => handleError(error));
```

```scheme
;; Chez Scheme - Continuation
(define (fetch-data return)
  (call/cc
   (lambda (k)
     ;; async operation simulation
     (return data))))

(call/cc
 (lambda (k)
   (let ([data (fetch-data k)])
     (process data))))
```

---

## Java vs Chez Scheme

### 类和对象 vs 记录

```java
// Java
public class Person {
    private String name;
    private int age;
    
    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
    
    public String getName() {
        return name;
    }
    
    public int getAge() {
        return age;
    }
}

Person p = new Person("Alice", 30);
System.out.println(p.getName());
```

```scheme
;; Chez Scheme
(define-record-type person
  (fields name age)
  (protocol
   (lambda (new)
     (lambda (name age)
       (new name age)))))

(define p (make-person "Alice" 30))
(person-name p)  ; => "Alice"
```

### 接口 vs 泛型函数

```java
// Java
interface Drawable {
    void draw();
}

class Circle implements Drawable {
    public void draw() {
        System.out.println("Drawing circle");
    }
}

class Square implements Drawable {
    public void draw() {
        System.out.println("Drawing square");
    }
}
```

```scheme
;; Chez Scheme - 多态通过记录类型
(define-record-type shape
  (fields draw-proc)
  (protocol
   (lambda (new)
     (lambda (draw-proc)
       (new draw-proc)))))

(define (draw shape)
  ((shape-draw-proc shape)))

(define circle 
  (make-shape 
   (lambda () (display "Drawing circle"))))

(define square 
  (make-shape 
   (lambda () (display "Drawing square"))))
```

---

## C++ vs Chez Scheme

### 模板 vs 宏

```cpp
// C++ - 模板
template<typename T>
T max(T a, T b) {
    return (a > b) ? a : b;
}

int result = max(10, 20);
```

```scheme
;; Chez Scheme - 泛型函数
(define (max a b)
  (if (> a b) a b))

(define result (max 10 20))

;; 或使用宏进行编译时优化
(define-syntax max
  (syntax-rules ()
    [(_ a b)
     (let ([temp-a a]
           [temp-b b])
       (if (> temp-a temp-b) temp-a temp-b))]))
```

### RAII vs with-* 模式

```cpp
// C++ - RAII
{
    std::ifstream file("data.txt");
    // file automatically closed when out of scope
}
```

```scheme
;; Chez Scheme - call-with-*
(call-with-input-file "data.txt"
  (lambda (port)
    ;; port automatically closed after
    (get-line port)))
```

---

## Haskell vs Chez Scheme

### 类型系统

```haskell
-- Haskell - 静态类型
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

data Maybe a = Nothing | Just a
```

```scheme
;; Chez Scheme - 动态类型
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;; "Maybe" 通过约定（#f 或值）
(define (safe-divide a b)
  (if (= b 0)
      #f
      (/ a b)))
```

### 惰性求值

```haskell
-- Haskell - 默认惰性
ones = 1 : ones
take 5 ones  -- [1,1,1,1,1]
```

```scheme
;; Chez Scheme - 显式惰性
(define-syntax delay
  (syntax-rules ()
    [(_ expr) (lambda () expr)]))

(define (force promise)
  (promise))

(define-syntax cons-stream
  (syntax-rules ()
    [(_ a b) (cons a (delay b))]))

(define (ones)
  (cons-stream 1 (ones)))

(define (stream-take n stream)
  (if (= n 0)
      '()
      (cons (car stream)
            (stream-take (- n 1) (force (cdr stream))))))
```

---

## Ruby vs Chez Scheme

### 代码块 vs Lambda

```ruby
# Ruby
[1, 2, 3].each do |x|
  puts x
end

result = [1, 2, 3].map { |x| x * 2 }
```

```scheme
;; Chez Scheme
(for-each
 (lambda (x)
   (display x)
   (newline))
 '(1 2 3))

(define result
  (map (lambda (x) (* x 2)) '(1 2 3)))
```

### 元编程

```ruby
# Ruby - method_missing
class DynamicHandler
  def method_missing(method, *args)
    puts "Called: #{method}"
  end
end
```

```scheme
;; Chez Scheme - 宏
(define-syntax define-dynamic-handler
  (syntax-rules ()
    [(_ name)
     (define name
       (lambda (method . args)
         (display "Called: ")
         (display method)
         (newline)))]))
```

---

## Common Lisp vs Chez Scheme

### 宏系统

```lisp
;; Common Lisp - defmacro
(defmacro when (test &rest body)
  `(if ,test
       (progn ,@body)))
```

```scheme
;; Chez Scheme - syntax-rules (卫生宏)
(define-syntax when
  (syntax-rules ()
    [(_ test body ...)
     (if test (begin body ...))]))

;; syntax-case (更强大)
(define-syntax when
  (lambda (x)
    (syntax-case x ()
      [(_ test body ...)
       #'(if test (begin body ...))])))
```

### 包系统 vs 库系统

```lisp
;; Common Lisp
(defpackage :myapp
  (:use :cl)
  (:export :main))

(in-package :myapp)
```

```scheme
;; Chez Scheme
(library (myapp)
  (export main)
  (import (chezscheme))
  
  ...)
```

---

## Clojure vs Chez Scheme

### 不可变数据结构

```clojure
;; Clojure - 默认不可变
(def v [1 2 3])
(def v2 (conj v 4))  ; v 不变，v2 是新向量
```

```scheme
;; Chez Scheme - 可变/不可变混合
;; 列表默认不可变
(define lst '(1 2 3))
(define lst2 (cons 4 lst))  ; lst 不变

;; 向量可变
(define vec (vector 1 2 3))
(vector-set! vec 0 10)  ; vec 被修改
```

### 并发原语

```clojure
;; Clojure - atom
(def counter (atom 0))
(swap! counter inc)
@counter  ; => 1
```

```scheme
;; Chez Scheme - 需要手动实现
(define counter 0)
(define counter-mutex (make-mutex))

(define (inc-counter!)
  (with-mutex counter-mutex
    (set! counter (+ counter 1))))
```

---

## 语言特性对比表

| 特性 | Python | JavaScript | Java | C++ | Haskell | Ruby | Common Lisp | Clojure | Chez Scheme |
|------|--------|------------|------|-----|---------|------|-------------|---------|-------------|
| 类型系统 | 动态 | 动态 | 静态 | 静态 | 静态 | 动态 | 动态 | 动态 | 动态 |
| 范式 | 多范式 | 多范式 | OOP | 多范式 | 函数式 | OOP | 多范式 | 函数式 | 函数式 |
| 宏 | 无 | 无 | 无 | 预处理器 | 有限 | 元编程 | 强大 | 强大 | 卫生宏 |
| 尾调用优化 | 无 | 部分 | 无 | 有时 | 是 | 无 | 部分 | 是 | 保证 |
| Continuation | 无 | 无 | 无 | 无 | 无 | 无 | 有 | 无 | 一级 |
| 惰性求值 | 生成器 | 生成器 | Stream | 无 | 默认 | Enumerator | 无 | 序列 | 手动 |
| 并发 | 线程/异步 | 异步 | 线程 | 线程 | STM | 线程 | 线程 | STM | 线程 |
| 性能 | 中等 | 快 | 快 | 很快 | 快 | 中等 | 快 | 快 | 很快 |

---

## 关键概念映射

### 循环
- **Python**: `for`, `while`
- **JavaScript**: `for`, `while`, `forEach`
- **Java**: `for`, `while`, `Stream.forEach`
- **Scheme**: 尾递归, `do`, `for-each`

### 异常处理
- **Python**: `try`/`except`
- **JavaScript**: `try`/`catch`
- **Java**: `try`/`catch`
- **Scheme**: `guard`

### 模块/包
- **Python**: `import`, packages
- **JavaScript**: `import`/`export`, modules
- **Java**: `import`, packages
- **Scheme**: `(import ...)`, libraries

### 函数定义
- **Python**: `def func():`
- **JavaScript**: `function func()`, `() => {}`
- **Java**: `type func() {}`
- **Scheme**: `(define (func) ...)`

---

## 总结

Chez Scheme 的独特之处：

1. **卫生宏系统**: 比其他 Lisp 更安全的宏
2. **保证的尾调用优化**: 真正的函数式编程
3. **一级 Continuation**: 强大的控制流抽象
4. **高性能**: 接近 C 的性能
5. **完整的 R6RS 支持**: 标准化的语言

从其他语言迁移到 Chez Scheme 的关键是理解函数式编程范式和 Lisp 的括号语法。一旦掌握，你会发现它是一个强大而优雅的编程工具。
