# 一个高效的R数据处理框架

实现这个**R**语言工具包，是为了方便管理R数据处理的任务流。

## 一、功能特性

-   支持一套成熟的 **R** 数据处理文件夹约定：取数任务、加工脚本、结果输出
-   支持一套任务流管理约定：批量导入、增量导入、针对数据集处理
-   使用 **Apache Parquet** 作为数据集的存储格式
-   数据集的磁盘读写和内存处理无序列化过程，非常高效
-   读取数据集支持列存储、文件分区和行组优化
-   写入数据集时支持增量写入，且仅保存修改过的分区文件
-   可识别最近修改过的数据集分区，方便做增量处理

[Apache Arrow](https://arrow.apache.org/) 库内置了[Apache Parquet存储格式](https://github.com/apache/parquet-format) ，并且包含了很多种语言的实现，包括 R语言的[arrow](https://arrow.apache.org/docs/r/)包。

## 二、安装和使用

### 1、安装

先从**github**上安装**glimmer**包：

```{r}
devtools::install_github("https://github.com/arcstep/glimmer")
```

### 2、使用

引入**glimmer**包后，应当设置四个文件夹位置。

```{r}
library(glimmer)
set_topic("STATE", "~/glimmer/STATE")
set_topic("IMPORT", "~/glimmer/IMPORT")
set_topic("TASK/IMPORT", "~/glimmer/TASK/IMPORT")
set_topic("TASK/BUILD", "~/glimmer/TASK/BUILD")
set_topic("CACHE", "~/glimmer/CACHE")
```

这初看起来啰嗦，但至少清楚明白。 实际上让你有能力灵活地切换处理目标。

## 三、使用指南：加工任务

使用**glimmer**包的数据处理框架，主要是两个步骤：

（1）将数据处理脚本保存在**TASK/BUILD**文件夹中

（3）执行批处理，然在**CACHE**文件夹中查看处理结果

下面实现一个简单的数据处理例子。

### 1、准备一个处理脚本（在 TASK/BUILD 中执行）

我们准备一个最简单的处理脚本。

脚本应当保存在 `~/glimmer/BUILD` 中。

脚本内容如下：

```{r}
mtcars |> as_tibble() |> write_dataset("this_is_my_dataset")
```

### 2、执行任务（结果保存在 CACHE）

执行如下脚本就可以导入上面的数据，并生成新的数据集。

```{r}
run_task_scripts(taskScript = "TASK/BUILD")
```

可以使用`read_dataset("this_is_my_dataset") |> collect()`查看刚刚添加过的数据集。

## 四、使用指南：导入任务

使用**glimmer**包的导入数据处理框架，主要是三个步骤：

（1）将导入数据保存在**IMPORT**文件夹中

（2）将处理脚本保存在**TASK/IMPORT**文件夹中

（3）执行处理批处理脚本，然后在**CACHE**文件夹中查看处理结果

下面实现一个导入数据的示例。

### 1、准备导入文件（从 IMPORT 导入）

我们要在`~/glimmer/IMPORT文件夹`中准备一个 **CSV** 文件：

```{csv}
a, b,
1, 2,
3, 4
```

接下来，将其保存在`~/glimmer/IMPORT/task1/mycsv/1.csv`的位置。

请注意，文件名之前有两层文件夹`task1`和`mycsv`。

**glimmer**包通过约定替代接口配置。上面保存导入数据时已经使用了约定：用`task1`表示**导入任务**，用`mycsv`表示要导入的**数据集**名称。

面对实际问题时，例如爬虫任务，往往是每天或每小时都有新的任务文件夹需要处理，且每个任务文件夹中也包含多个数据集。 一次任务中，也可能包含几个、几十个甚至几百个要处理的导入数据集。你实际要处理的数据集可能长这样：

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

回到我们刚才的示例，保存好的导入文件结构应该长这样：

```{bash}
~/glimmer/IMPORT/task1/mycsv/1.csv
```

### 2、准备一个处理脚本（在 TASK/IMPORT 中执行）

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

### 3、执行任务（结果保存在 CACHE）

如果是首次运行，执行如下脚本就可以导入上面的数据，并生成新的数据集。

```{r}
taskfolder_todo()
```

这一命令是增量运行的，即使运行多次，之前准备的脚本也只会被运行一次。这是因为，导入文件夹`task1`已经被处理的状态被记录到**STATE**文件夹中了，如果删除其中的那条记录或整个文件夹，则上面的命令就可以重新生效。

增量运行会很有用，但有时候仍然希望重新处理。使用`taskfolder_redo("task1")`可以再次处理`task1`文件夹的数据，即使`task1`已经被处理过。如果要重新处理多个任务文件夹，则可以使用字符串向量作为参数实现批处理，例如：`taskfolder_redo(c("task1", "task2"))`。

可以使用`read_dataset("mycsv") |> collect()`查看刚刚添加过的数据集。

如果你想看看都曾经保存过哪些数据集，可以使用`all_datasets()`。

如果没有报错，那么恭喜你：成功部署了一个数据导入任务！
