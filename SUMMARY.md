# Chez Scheme 教程总结

本仓库提供了一套完整、深入的 Chez Scheme 教程，专为已熟悉其他 Lisp 方言的高级程序员设计。

## 📚 学习资源总览

### 主要教程
- **[tutorial.md](tutorial.md)** (1,808 行)
  - 12 章完整教程
  - 从基础到高级的系统性讲解
  - 包含大量实例和最佳实践

### 快速参考
- **[README.md](README.md)** - 项目概览和快速开始
- **[QUICK-REFERENCE.md](QUICK-REFERENCE.md)** - 语法速查手册
- **[LANGUAGE-COMPARISON.md](LANGUAGE-COMPARISON.md)** - 与其他语言的对比

### 实践指南
- **[EXERCISES.md](EXERCISES.md)** - 14 个练习题（含详细答案）
- **[PROJECT-TEMPLATE.md](PROJECT-TEMPLATE.md)** - 项目结构模板

## 💻 示例代码

8 个完整的可运行示例：

1. **[01-basic-syntax.ss](examples/01-basic-syntax.ss)** (180 行)
   - 数据类型、控制流、函数
   - 列表操作、高阶函数
   
2. **[02-macros.ss](examples/02-macros.ss)** (180 行)
   - syntax-rules 和 syntax-case
   - 模式匹配、递归宏
   
3. **[03-records.ss](examples/03-records.ss)** (229 行)
   - 记录类型定义和使用
   - 继承、图书管理系统实例
   
4. **[04-continuations.ss](examples/04-continuations.ss)** (287 行)
   - call/cc 基础和应用
   - 生成器、协程、回溯搜索
   
5. **[05-performance.ss](examples/05-performance.ss)** (276 行)
   - Fixnum/Flonum 优化
   - 记忆化、尾递归、内联
   
6. **[06-library-usage.ss](examples/06-library-usage.ss)** (150 行)
   - 自定义库的使用
   - 模块化编程示例
   
7. **[07-file-io.ss](examples/07-file-io.ss)** (271 行)
   - 文本/二进制文件读写
   - CSV 处理、格式化输出
   
8. **[08-utilities.ss](examples/08-utilities.ss)** (335 行)
   - 命令行参数、环境变量
   - 时间日期、哈希表、字符串操作

## 🎯 学习路径

### 初学者路径（有其他 Lisp 经验）
1. 阅读 [README.md](README.md) 了解概况
2. 浏览 [QUICK-REFERENCE.md](QUICK-REFERENCE.md) 熟悉语法
3. 运行 [01-basic-syntax.ss](examples/01-basic-syntax.ss)
4. 完成 [EXERCISES.md](EXERCISES.md) 前 5 个练习
5. 深入 [tutorial.md](tutorial.md) 第 1-6 章

### 中级路径（想深入理解）
1. 学习 [tutorial.md](tutorial.md) 第 4 章（宏系统）
2. 运行 [02-macros.ss](examples/02-macros.ss)
3. 完成 [EXERCISES.md](EXERCISES.md) 练习 4-7
4. 学习 [04-continuations.ss](examples/04-continuations.ss)
5. 完成 [EXERCISES.md](EXERCISES.md) 练习 8-9

### 高级路径（追求性能和实战）
1. 研究 [tutorial.md](tutorial.md) 第 7、9 章
2. 分析 [05-performance.ss](examples/05-performance.ss)
3. 完成 [EXERCISES.md](EXERCISES.md) 练习 10-14
4. 使用 [PROJECT-TEMPLATE.md](PROJECT-TEMPLATE.md) 构建项目
5. 探索 FFI 和并发（tutorial.md 第 6、8 章）

## 📊 内容统计

| 类别 | 文件数 | 总行数 | 说明 |
|------|--------|--------|------|
| 核心教程 | 1 | 1,808 | 完整的理论讲解 |
| 示例代码 | 8 | 1,908 | 可运行的实例 |
| 参考文档 | 4 | 2,784 | 速查和对比 |
| 库文件 | 2 | 182 | 自定义库 |
| **总计** | **15** | **6,682** | |

## 🎓 核心主题覆盖

### 语言特性
- ✅ 数据类型和结构
- ✅ 控制流和循环
- ✅ 函数和闭包
- ✅ 宏系统（syntax-rules 和 syntax-case）
- ✅ 记录类型和 OOP 模式
- ✅ Continuation（call/cc）
- ✅ 模块和库系统

### 高级主题
- ✅ 性能优化技巧
- ✅ FFI（C 语言集成）
- ✅ 并发和线程
- ✅ 内存管理和 GC
- ✅ 调试和剖析

### 实践技能
- ✅ 文件 I/O 操作
- ✅ 字符串和列表处理
- ✅ 哈希表和数据结构
- ✅ 命令行工具开发
- ✅ 项目组织和测试

## 🔧 工具和资源

### 开发工具
- Chez Scheme REPL
- 编译器优化
- 调试工具（trace, break, time）
- 性能分析工具

### 外部资源
- [官方文档](https://cisco.github.io/ChezScheme/)
- [The Scheme Programming Language (4th Ed)](https://www.scheme.com/tspl4/)
- [R6RS 标准](http://www.r6rs.org/)
- [GitHub 仓库](https://github.com/cisco/ChezScheme)

## 🚀 快速开始

```bash
# 安装 Chez Scheme
sudo apt-get install chezscheme  # Ubuntu/Debian
brew install chezscheme          # macOS

# 克隆教程
git clone https://github.com/zhaoyul/chez-tutorial.git
cd chez-tutorial

# 运行第一个示例
scheme --script examples/01-basic-syntax.ss

# 启动 REPL
scheme
```

## 🎯 适合人群

✅ 已熟悉其他 Lisp 方言（Common Lisp、Clojure、Racket）  
✅ 有函数式编程经验  
✅ 想学习高性能 Scheme 实现  
✅ 对编程语言实现感兴趣  
✅ 需要系统学习宏编程  

## 🌟 特色亮点

1. **全中文教程**：完整的中文讲解，降低学习门槛
2. **面向高级程序员**：直击核心，不浪费时间
3. **理论与实践结合**：每个概念都有可运行示例
4. **练习题丰富**：14 个练习覆盖所有主题
5. **项目模板**：直接可用的项目结构
6. **语言对比**：快速理解与其他语言的异同

## 📝 学习建议

1. **循序渐进**：按照推荐路径学习，不要跳跃
2. **动手实践**：运行每个示例，修改并观察结果
3. **完成练习**：练习题是检验理解的最好方式
4. **构建项目**：用学到的知识构建实际项目
5. **参考文档**：遇到问题查阅快速参考和官方文档
6. **社区交流**：分享经验，解答疑问

## 🤝 贡献

欢迎贡献！可以：
- 报告错误或提出改进建议
- 添加新的示例或练习
- 改进文档和注释
- 分享你的学习心得

## 📄 许可证

本教程采用 MIT 许可证。

---

**开始你的 Chez Scheme 之旅吧！** 🚀

从 [README.md](README.md) 开始，或直接跳到 [tutorial.md](tutorial.md)！
