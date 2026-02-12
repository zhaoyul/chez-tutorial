# Chez Scheme 高级教程

欢迎来到 Chez Scheme 高级教程！本教程专为已经熟悉其他 Lisp 方言的高级程序员设计，深入介绍 Chez Scheme 的独特特性和高级功能。

## 简介

Chez Scheme 是由 R. Kent Dybvig 开发的高性能 Scheme 实现，现已开源并被 Cisco 维护。它以极高的性能、完整的 R6RS 支持和强大的宏系统而闻名。

### 主要特点

- **极高性能**：原生代码编译器，接近 C 语言性能
- **完整的 R6RS 支持**：完全符合 R6RS 标准
- **增量编译**：支持 REPL 中的快速开发
- **强大的宏系统**：syntax-case 宏系统（卫生宏）
- **成熟的 FFI**：与 C 语言的无缝集成
- **精确的垃圾回收**：世代垃圾回收器

## 教程结构

本教程包含以下内容：

### 主教程文档

📖 **[tutorial.md](tutorial.md)** - 完整的深入教程，涵盖：

1. **简介和环境配置** - Chez Scheme 的特点和安装
2. **核心语言特性** - 数据类型、控制流、函数、闭包、continuation
3. **宏系统** - syntax-rules 和 syntax-case 的深入讲解
4. **模块和库系统** - R6RS 库系统的使用
5. **外部函数接口 (FFI)** - 与 C 语言的集成
6. **性能优化** - 编译器优化、类型声明、性能剖析
7. **并发和并行** - 线程、同步、并行计算
8. **内存管理** - 垃圾回收、弱引用、守护者
9. **实战示例** - Web 服务器、JSON 解析器、测试框架
10. **调试和剖析** - 调试工具和性能分析
11. **与其他实现的差异** - 与 Racket、Guile、Chicken 的比较
12. **最佳实践** - 代码组织、错误处理、性能考虑

### 示例代码

📁 **[examples/](examples/)** 目录包含可运行的示例代码：

- **[01-basic-syntax.ss](examples/01-basic-syntax.ss)** - 基础语法示例
  - 数据类型、控制流、函数、列表操作、高阶函数
  
- **[02-macros.ss](examples/02-macros.ss)** - 宏系统示例
  - syntax-rules、模式匹配、递归宏、DSL 创建
  
- **[03-records.ss](examples/03-records.ss)** - 记录类型示例
  - 基本记录、可变字段、继承、实际应用（图书管理系统）
  
- **[04-continuations.ss](examples/04-continuations.ss)** - Continuation 示例
  - call/cc 基础、非局部退出、生成器、协程、异常处理、回溯搜索
  
- **[05-performance.ss](examples/05-performance.ss)** - 性能优化示例
  - Fixnum 操作、向量 vs 列表、尾递归、内联、记忆化
  
- **[06-library-usage.ss](examples/06-library-usage.ss)** - 库使用示例
  - 导入和使用自定义库
  
- **[mylib.sls](examples/mylib.sls)** - 自定义库实现
  - 数学库、字符串工具、数据结构（栈）

## 快速开始

### 1. 安装 Chez Scheme

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

### 2. 运行 REPL

```bash
$ scheme
Chez Scheme Version 9.5.8
Copyright 1984-2022 Cisco Systems, Inc.

> (+ 1 2 3)
6
```

### 3. 运行示例代码

```bash
# 运行基础语法示例
scheme --script examples/01-basic-syntax.ss

# 运行宏系统示例
scheme --script examples/02-macros.ss

# 运行记录类型示例
scheme --script examples/03-records.ss

# 运行 continuation 示例
scheme --script examples/04-continuations.ss

# 运行性能优化示例
scheme --script examples/05-performance.ss

# 运行库使用示例（需要先编译库）
scheme --script examples/06-library-usage.ss
```

## 学习路径

### 如果你熟悉 Common Lisp

重点关注：
- 宏系统的差异（syntax-case vs defmacro）
- 模块系统（R6RS 库 vs 包）
- 尾调用优化保证
- Continuation 的使用

### 如果你熟悉 Clojure

重点关注：
- 不可变性不是默认的（但可以实现）
- 没有内置的并发原语（如 atom、agent）
- 宏系统更传统但更卫生
- 更接近标准 Scheme

### 如果你熟悉其他 Scheme 实现

重点关注：
- Chez 特定的性能优化
- 强大的 FFI 系统
- 编译器和运行时特性
- 扩展的标准库

## 目标受众

本教程适合：

✅ 已经熟悉其他 Lisp 方言的程序员  
✅ 有函数式编程经验的开发者  
✅ 想要学习高性能 Scheme 实现的人  
✅ 对编程语言实现感兴趣的研究者  

不适合：
❌ 完全的编程初学者（建议先学习基础的 Scheme）  
❌ 寻找实用库和框架的人（Chez 生态相对较小）

## 参考资源

### 官方文档

- [The Scheme Programming Language (4th Edition)](https://www.scheme.com/tspl4/) - R. Kent Dybvig
- [The Chez Scheme User's Guide](https://cisco.github.io/ChezScheme/csug9.5/)
- [R6RS Scheme 标准](http://www.r6rs.org/)

### Chez Scheme

- [官方网站](https://cisco.github.io/ChezScheme/)
- [GitHub 仓库](https://github.com/cisco/ChezScheme)

### 推荐书籍

- **Structure and Interpretation of Computer Programs (SICP)** - 经典的计算机科学教材
- **The Little Schemer** 系列 - 通过问答形式学习 Scheme
- **Essentials of Programming Languages (EOPL)** - 深入讲解编程语言实现

## 贡献

欢迎贡献！如果你发现错误或有改进建议，请：

1. Fork 本仓库
2. 创建你的特性分支
3. 提交你的改动
4. 推送到分支
5. 创建 Pull Request

## 许可证

本教程采用 MIT 许可证。详见 LICENSE 文件。

## 联系方式

如有问题或建议，欢迎提交 Issue。

---

**开始学习：** [阅读完整教程 →](tutorial.md)

**运行示例：** `scheme --script examples/01-basic-syntax.ss`