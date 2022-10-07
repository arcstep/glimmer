# 使用glimmer高效处理数据

## 一、为什么使用**glimmer**？

在实验环境中，R脚本可以凭借RStudio、交互控制台、RMarkdown等工具环境迅速进入工作状态， 再凭借丰富的数据处理库、机器学习库、可视化图表库等开展数据分析工作。

但将成果迁移至生产环境就会付出巨大代价。 例如，实验环境和交互式环境下非常鼓励自动推断数据类型，但生产环境更需要确定的数据类型。

创建这个**glimmer**这个R语言工具包，就是为了衔接实验环境和生产环境， 使得RStudio工作台下的成果能够迅速转换为生产环境中的任务流。

**glimmer**具有的特性：

-   使用一套文件夹约定：导入素材、加工脚本、任务、数据集
-   使用 **Apache Parquet** 作为数据集的存储格式：读取数据集支持列存储、文件分区和行组优化，磁盘读写和内存处理无序列化过程
-   提供了一组数据访问方法，支持新增、修改、删除、查询等操作
-   支持任务自定义：脚本字符串、R文件、R文件夹
-   支持任务批处理：批量建立、统一执行

glimmer重度使用了**Apache Arrow**库，并使用其中的**Apache Parquet**格式作为文件存储。 **Apache Parquet**库内置了[Apache Parquet存储格式](https://github.com/apache/parquet-format) ，并且包含了很多种语言的实现，包括R语言的[arrow](https://arrow.apache.org/docs/r/)包。 更多信息请参考[Apache Arrow](https://arrow.apache.org/)。

## 二、安装

直接从**github**上安装**glimmer**包：

```{r}
remotes::install_github("arcstep/glimmer")
```

## 三、使用指南

### 1 工作目录寻址：快速访问工作文件夹

执行**config_init**函数后，就拥有了一个工作环境。

```{r}
library(glimmer)
config_init("./")
get_path("CACHE", "abc") |> fs::dir_ls()
```

或使用临时文件夹`config_init(tempdir())`：

    $CACHE
    /var/folders/f5/rlf27f4n6wzc_k4x7y4vzm5h0000gn/T/RtmpCqRew4/CACHE

    $IMPORT
    /var/folders/f5/rlf27f4n6wzc_k4x7y4vzm5h0000gn/T/RtmpCqRew4/IMPORT

    $TASK_SCRIPTS
    /var/folders/f5/rlf27f4n6wzc_k4x7y4vzm5h0000gn/T/RtmpCqRew4/TASK_SCRIPTS

    $TASK_DEFINE
    /var/folders/f5/rlf27f4n6wzc_k4x7y4vzm5h0000gn/T/RtmpCqRew4/TASK_DEFINE

### 2 数据集读写：架构定义、列式存取、懒惰计算

### 3 任务定义：从函数库构建、自定义、可配置

### 4 任务调度：队列批处理、导入流程
