# Chez Scheme 项目模板

这是一个标准的 Chez Scheme 项目结构模板。

## 目录结构

```
my-project/
├── README.md           # 项目说明
├── LICENSE             # 许可证
├── .gitignore          # Git 忽略文件
├── Makefile            # 构建脚本（可选）
├── src/                # 源代码目录
│   ├── main.ss         # 主程序
│   └── mylib.sls       # 库文件
├── tests/              # 测试目录
│   └── test-all.ss     # 测试套件
├── docs/               # 文档目录
│   └── api.md          # API 文档
└── examples/           # 示例代码
    └── example1.ss     # 示例程序
```

## 项目文件示例

### src/mylib.sls - 库文件

```scheme
(library (mylib)
  (export 
    ;; 核心函数
    process-data
    validate-input
    
    ;; 工具函数
    string-utils
    list-utils)
  
  (import (chezscheme))
  
  ;; 数据处理函数
  (define (process-data data)
    (unless (valid-data? data)
      (error 'process-data "invalid data" data))
    ;; 处理逻辑
    (map transform-item data))
  
  (define (transform-item item)
    ;; 转换逻辑
    item)
  
  (define (valid-data? data)
    (and (list? data)
         (not (null? data))))
  
  ;; 输入验证
  (define (validate-input input)
    (cond
      [(not (string? input))
       (error 'validate-input "input must be string")]
      [(string=? input "")
       (error 'validate-input "input cannot be empty")]
      [else input]))
  
  ;; 字符串工具
  (define string-utils
    (let ()
      (define (trim str)
        ;; 去除首尾空白
        str)
      
      (define (split str delim)
        ;; 分割字符串
        '())
      
      ;; 返回工具对象
      (lambda (op . args)
        (case op
          [(trim) (apply trim args)]
          [(split) (apply split args)]
          [else (error 'string-utils "unknown operation" op)]))))
  
  ;; 列表工具
  (define list-utils
    (let ()
      (define (take lst n)
        (if (or (null? lst) (<= n 0))
            '()
            (cons (car lst) (take (cdr lst) (- n 1)))))
      
      (define (drop lst n)
        (if (or (null? lst) (<= n 0))
            lst
            (drop (cdr lst) (- n 1))))
      
      ;; 返回工具对象
      (lambda (op . args)
        (case op
          [(take) (apply take args)]
          [(drop) (apply drop args)]
          [else (error 'list-utils "unknown operation" op)]))))
  
  ) ;; end library
```

### src/main.ss - 主程序

```scheme
#!/usr/bin/env scheme-script
;; 主程序

(import (chezscheme)
        (mylib))

;; 配置
(define *config*
  '((debug . #t)
    (verbose . #f)
    (output-dir . "/tmp")))

(define (get-config key)
  (cdr (assoc key *config*)))

;; 主函数
(define (main args)
  (display "欢迎使用 My Project\n")
  
  ;; 解析命令行参数
  (when (null? args)
    (display "用法: main.ss <input-file>\n")
    (exit 1))
  
  (let ([input-file (car args)])
    ;; 验证输入
    (unless (file-exists? input-file)
      (error 'main "文件不存在" input-file))
    
    ;; 读取和处理数据
    (let ([data (read-data input-file)])
      (when (get-config 'debug)
        (display "读取了 ")
        (display (length data))
        (display " 条记录\n"))
      
      ;; 处理数据
      (let ([result (process-data data)])
        ;; 输出结果
        (write-results result)
        (display "处理完成！\n")))))

;; 读取数据
(define (read-data filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ([items '()])
        (let ([line (get-line port)])
          (if (eof-object? line)
              (reverse items)
              (loop (cons line items))))))))

;; 写入结果
(define (write-results results)
  (let ([output-file (string-append (get-config 'output-dir) "/output.txt")])
    (call-with-output-file output-file
      (lambda (port)
        (for-each
         (lambda (item)
           (fprintf port "~a\n" item))
         results))
      'replace)
    (display "结果已写入: ")
    (display output-file)
    (newline)))

;; 运行主函数
(main (cdr (command-line)))
```

### tests/test-all.ss - 测试套件

```scheme
#!/usr/bin/env scheme-script
;; 测试套件

(import (chezscheme)
        (mylib))

;; 测试框架
(define *test-count* 0)
(define *test-passed* 0)
(define *test-failed* 0)

(define-syntax test
  (syntax-rules ()
    [(_ name body ...)
     (begin
       (set! *test-count* (+ *test-count* 1))
       (display "测试 ")
       (display *test-count*)
       (display ": ")
       (display name)
       (display " ... ")
       (guard (ex
               [else
                (set! *test-failed* (+ *test-failed* 1))
                (display "失败\n")
                (display "  错误: ")
                (display (condition-message ex))
                (newline)])
         (begin body ...)
         (set! *test-passed* (+ *test-passed* 1))
         (display "通过\n")))]))

(define-syntax assert-equal
  (syntax-rules ()
    [(_ expected actual)
     (unless (equal? expected actual)
       (error 'assert-equal
              (format "期望 ~s 但得到 ~s" expected actual)))]))

(define-syntax assert-true
  (syntax-rules ()
    [(_ expr)
     (unless expr
       (error 'assert-true "期望真值"))]))

(define-syntax assert-false
  (syntax-rules ()
    [(_ expr)
     (when expr
       (error 'assert-false "期望假值"))]))

;; 测试用例
(define (run-tests)
  (display "运行测试...\n\n")
  
  ;; 测试 validate-input
  (test "validate-input - 有效输入"
    (assert-equal "hello" (validate-input "hello")))
  
  (test "validate-input - 空字符串应该失败"
    (guard (ex [else #t])
      (validate-input "")
      (error 'test "应该抛出异常")))
  
  ;; 测试 process-data
  (test "process-data - 有效列表"
    (assert-true (list? (process-data '(1 2 3)))))
  
  (test "process-data - 空列表应该失败"
    (guard (ex [else #t])
      (process-data '())
      (error 'test "应该抛出异常")))
  
  ;; 测试 list-utils
  (test "list-utils - take"
    (assert-equal '(1 2 3) (list-utils 'take '(1 2 3 4 5) 3)))
  
  (test "list-utils - drop"
    (assert-equal '(4 5) (list-utils 'drop '(1 2 3 4 5) 3)))
  
  ;; 显示结果
  (newline)
  (display "测试完成\n")
  (display "总计: ") (display *test-count*) (newline)
  (display "通过: ") (display *test-passed*) (newline)
  (display "失败: ") (display *test-failed*) (newline)
  
  ;; 返回退出码
  (if (= *test-failed* 0) 0 1))

;; 运行测试
(exit (run-tests))
```

### Makefile - 构建脚本

```makefile
# Chez Scheme 项目 Makefile

SCHEME := scheme
SRC_DIR := src
TEST_DIR := tests
BUILD_DIR := build

# 源文件
SOURCES := $(wildcard $(SRC_DIR)/*.ss $(SRC_DIR)/*.sls)
COMPILED := $(SOURCES:%.ss=$(BUILD_DIR)/%.so)

.PHONY: all build test clean run install

# 默认目标
all: build

# 编译所有源文件
build: $(BUILD_DIR)
	@echo "编译项目..."
	@for file in $(SOURCES); do \
		echo "编译 $$file..."; \
		$(SCHEME) --compile-imported-libraries --program $$file; \
	done
	@echo "编译完成"

# 创建构建目录
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# 运行测试
test:
	@echo "运行测试..."
	@$(SCHEME) --script $(TEST_DIR)/test-all.ss

# 运行主程序
run:
	@$(SCHEME) --script $(SRC_DIR)/main.ss

# 清理
clean:
	@echo "清理..."
	@rm -rf $(BUILD_DIR)
	@find . -name "*.so" -delete
	@echo "清理完成"

# 安装（示例）
install:
	@echo "安装到系统..."
	@# 添加安装逻辑

# 帮助
help:
	@echo "可用目标:"
	@echo "  all     - 构建项目（默认）"
	@echo "  build   - 编译所有源文件"
	@echo "  test    - 运行测试"
	@echo "  run     - 运行主程序"
	@echo "  clean   - 清理构建产物"
	@echo "  install - 安装程序"
	@echo "  help    - 显示此帮助信息"
```

### .gitignore

```gitignore
# 编译文件
*.so
*.sls.so
*.ss.so

# 构建目录
build/
dist/

# 临时文件
*~
*.bak
*.swp
.DS_Store

# IDE 文件
.vscode/
.idea/
*.sublime-*

# OS 文件
Thumbs.db

# 日志文件
*.log

# 测试输出
/tmp/output.txt
```

### README.md - 项目说明

```markdown
# My Project

简短的项目描述。

## 功能特性

- 特性 1
- 特性 2
- 特性 3

## 安装

```bash
# 克隆仓库
git clone https://github.com/username/my-project.git
cd my-project

# 安装依赖（如果有）
# ...
```

## 使用方法

```bash
# 运行程序
scheme --script src/main.ss input.txt

# 或使用 Makefile
make run
```

## 开发

```bash
# 运行测试
make test

# 编译项目
make build

# 清理
make clean
```

## API 文档

参见 [docs/api.md](docs/api.md)

## 许可证

MIT License
```

## 使用项目模板

1. **创建新项目**:
   ```bash
   mkdir my-project
   cd my-project
   # 复制模板文件
   ```

2. **修改项目信息**:
   - 更新 `README.md` 中的项目名称和描述
   - 修改 `src/mylib.sls` 中的库名称
   - 调整 `src/main.ss` 中的主逻辑

3. **开发和测试**:
   ```bash
   # 编写代码
   vim src/mylib.sls
   
   # 编写测试
   vim tests/test-all.ss
   
   # 运行测试
   make test
   ```

4. **构建和运行**:
   ```bash
   # 编译
   make build
   
   # 运行
   make run
   ```

## 最佳实践

1. **代码组织**:
   - 将可重用代码放在库中 (`src/*.sls`)
   - 主程序保持简洁 (`src/main.ss`)
   - 为每个模块编写测试

2. **错误处理**:
   - 使用 `guard` 处理异常
   - 提供有意义的错误消息
   - 验证所有输入

3. **性能**:
   - 使用尾递归
   - 考虑使用 fixnum/flonum 操作
   - 编译关键代码路径

4. **文档**:
   - 为公共 API 编写文档字符串
   - 保持 README 更新
   - 提供使用示例

5. **测试**:
   - 为核心功能编写单元测试
   - 测试边界情况
   - 使用 CI/CD 自动化测试

## 示例项目

参考以下真实项目的结构：

- Chez Web Server
- JSON Parser
- Database Wrapper
- CLI Tool

这个模板提供了一个良好的起点，可以根据具体需求进行调整。
