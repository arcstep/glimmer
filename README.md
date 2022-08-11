# 一个高效的R数据处理框架

实现这个**R**语言工具包，是为了方便管理数据处理的脚本任务。

## 一、功能特性

-   支持一套成熟的 **R** 数据处理文件夹约定：取数任务、加工脚本、结果输出
-   支持一套任务流管理约定：批量导入、增量导入、针对数据集处理
-   使用 **Apache Parquet** 作为数据集的存储格式
-   数据集的磁盘读写和内存处理无序列化过程，非常高效
-   读取数据集支持列存储、文件分区和行组优化
-   写入数据集时支持增量写入，且仅保存修改过的分区文件
-   可识别最近修改过的数据集分区，方便做增量处理

## 二、使用指南：导入任务

### 1、安装

可以从**github**上安装**glimmer**包：

```{r}
devtools::install_github("https://github.com/arcstep/glimmer")
```

### 2、使用

引入**glimmer**包后，应当设置四个文件夹位置。

```{r}
library(glimmer)
set_topic("STATE", "~/glimmer/STATE")
set_topic("IMPORT", "~/glimmer/IMPORT")
set_topic("TASK", "~/glimmer/TASK")
set_topic("CACHE", "~/glimmer/CACHE")
```

初看起来显得啰嗦，但至少非常清楚明白。 实际上，这很有实用价值，可以方便在实际脚本工作中灵活切换要处理的目录。

### 3、准备导入文件（从 IMPORT 导入）

如果你只是做一下测试，可以在`~/glimmer/IMPORT`中准备一个简单的 **CSV** 文件：

```{csv}
a, b,
1, 2,
3, 4
```

然后将其保存在`~/glimmer/IMPORT/task1/mycsv/1.csv`的位置。 请注意，中间增加了两层文件夹`task1`和`mycsv`。 这是一套约定机制，用`task1`表示一次导入任务，用`mycsv`表示要导入的数据集名称。

不要嫌麻烦，毕竟你可能需要每天或每小时都有新的任务文件夹需要处理，例如爬虫任务。 通过简单的约定，可以减少很多接口配置。 一次任务中，也可能包含几个、几十个甚至几百个要处理的导入数据集。

你实际要处理的数据集可能长这样：

```{bash}
~/glimmer/IMPORT/task1/t1/1.csv
~/glimmer/IMPORT/task1/t1/2.csv
~/glimmer/IMPORT/task1/t2/1.csv
~/glimmer/IMPORT/task1/t2/2.csv
~/glimmer/IMPORT/task1/...
~/glimmer/IMPORT/task2/t1/1.csv
~/glimmer/IMPORT/task2/t1/2.csv
~/glimmer/IMPORT/task2/t2/1.csv
~/glimmer/IMPORT/task2/t2/2.csv
...
(还有很多)
...
~/glimmer/IMPORT/task9/t1/1.csv
...
```

而我们刚刚建立的文件只是最简单的情况：

```{bash}
~/glimmer/IMPORT/task1/mycsv/1.csv
```

### 4、准备一个处理脚本（在 TASK 中执行）

我们同样准备一个最简单的处理脚本，导入数据，并为数据集增加一列。

脚本应当保存在 `~/glimmer/TASK` 中。

脚本内容如下：

```{r}
dsName <- "mycsv"
path <- get_path("IMPORT", get_topic("__DOING_TASK_FOLDER__"), dsName)
if(path |> fs::dir_exists()) {
  arrow::open_dataset(path, format = "csv") |>
    collect() |>
    mutate(c = a + b) |>
    write_dataset(dsName)
}
```

这一段 **R** 代码似乎有点多，但是一个很好的处理框架。

第1行是针对`mycsv`数据集来处理的，如果你有多个数据集就要在每段代码中清楚地指明。

第2行是一个几乎不修改的框架代码，除非你处理的数据不是来自于 **IMPORT** 这个默认主题。批处理时， `get_topic("__DOING_TASK_FOLDER__")` 就是当前迭代的目标文件夹，本例中的循环只有一次，当然就是taask1；而 `get_path`则是一个很方便的组装文件或文件夹路径的工具函数。

第3行是判断组装后的路径是否存在，如果任务文件夹中存在多种不同的数据集需要导入或加工，就可以精确匹配，跳过自己不用处理的部分。

第4行开始，就是具体的数据处理逻辑。

倒数第2行`write_dataset(dsName)`是将数据集按照原有的名字写入 **CACHE** 主题指定的文件夹。

如果不考虑可以解决大问题的处理框架，上面的代码也可以简化为：

```{r}
get_path("IMPORT", get_topic("__DOING_TASK_FOLDER__"), "mycsv") |>
  arrow::open_dataset(format = "csv") |>
  collect() |>
  mutate(c = a + b) |>
  write_dataset(dsName)
```

### 5、执行任务（结果保存在 CACHE）

执行如下脚本：

```{r}
taskfolder_redo("task1")
```

如果没有报错，那么恭喜你：成功部署了一个数据导入任务！

### 6、查看加工过的数据集

可以使用`read_dataset("mycsv")`查看刚刚添加过的数据集

如果你想看看都曾经保存过哪些数据集，可以使用`all_datasets()`。
