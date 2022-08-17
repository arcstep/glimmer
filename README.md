# 一个高效的R数据处理框架

实现这个**R**语言工具包，是为了方便管理R数据处理的任务流。

## 一、功能特性

-   支持一套成熟的 **R** 数据处理文件夹约定：导入素材、加工脚本、风险模型、数据集
-   支持一套任务流管理约定：批量导入、增量导入、数据加工、疑点筛查
-   使用 **Apache Parquet** 作为数据集的存储格式
-   读取数据集支持列存储、文件分区和行组优化，磁盘读写和内存处理无序列化过程
-   支持增量补写数据集，支持增量识别工作流

[Apache Arrow](https://arrow.apache.org/) 库内置了[Apache Parquet存储格式](https://github.com/apache/parquet-format) ，并且包含了很多种语言的实现，包括 R语言的[arrow](https://arrow.apache.org/docs/r/)包。

## 二、安装和使用

### 1、安装

先从**github**上安装**glimmer**包：

```{r}
devtools::install_github("https://github.com/arcstep/glimmer")
```

### 2、使用

引入**glimmer**包后，应当设置如下个文件夹位置。

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

（2）执行批处理，然在**CACHE**文件夹中查看处理结果

下面实现一个简单的数据处理例子。

### 1、准备一个处理脚本（在 TASK/BUILD 中执行）

我们准备一个最简单的处理脚本。

脚本应当保存在 `~/glimmer/BUILD` 中。

脚本内容如下：

```{r}
mtcars |> as_tibble() |> ds_write("this_is_my_dataset")
```

### 2、执行任务（结果保存在 CACHE）

执行如下脚本就可以导入上面的数据，并生成新的数据集。

```{r}
run_task(taskTopic = "TASK/BUILD")
```

可以使用`ds_read("this_is_my_dataset") |> collect()`查看刚刚添加过的数据集。

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

脚本应当保存在 `~/glimmer/TASK/IMPORT` 中。

脚本内容如下：

```{r}
ds_import("mycsv", function(path) {
  path |> arrow::open_dataset(format = "csv") |>
    collect() |>
    mutate(c = a + b) |>
    write_dataset("my_dataset")
})
```

**ds_import** 函数是很有用的帮助函数，上面也举例了便携导入任务的范式代码。

### 3、执行任务（结果保存在 CACHE）

如果是首次运行，执行如下脚本就可以导入上面的数据，并生成新的数据集。

```{r}
import_todo()
```

这一命令是增量运行的，即使运行多次，之前准备的脚本也只会被运行一次。这是因为，导入文件夹`task1`已经被处理的状态被记录到**STATE**文件夹中了，如果删除其中的那条记录或整个文件夹，则上面的命令就可以重新生效。

增量运行会很有用，但有时候仍然希望重新处理。使用`import_redo("task1")`可以再次处理`task1`文件夹的数据，即使`task1`已经被处理过。如果要重新处理多个任务文件夹，则可以使用字符串向量作为参数实现批处理，例如：`import_redo(c("task1", "task2"))`。

可以使用`ds_read("my_dataset") |> collect()`查看刚刚添加过的数据集。

如果你想看看都曾经保存过哪些数据集，可以使用`ds_all()`。

如果你想查看当前导入的任务文件夹，可以使用`get_importing_folder()`。

恭喜你：成功部署了一个数据导入任务！

## 五、关键概念

-   主题：根据任务主题约定的文件夹，在执行进程内有效

-   脚本主题：约定的脚本执行根文件夹

-   脚本文件夹：脚本主题文件夹内，所有包含可执行R文件的文件夹都可以作为脚本文件夹

-   导入主题：导入数据时约定的导入素材存放根文件夹

-   导入文件夹：导入素材的打包文件夹

-   缓存主题：约定好的存放数据集的根文件夹

-   数据集：在缓存主题文件夹下的存放数据的子文件夹，可以是多层级，数据集存储大多使用带有分区的 **Apache Parquet** 格式

-   数据集元文件：在数据集文件夹下存放的隐藏文件，以yml格式存储数据集的描述信息

-   数据集分区：数据集中 **Apache Parquet** 存储格式中的分区文件夹

-   风险模型：使用yml文件存储的数据集过滤模型

-   疑点数据集：风险模型运行后，将疑点筛查结果按照统一格式存储到疑点数据集

-   状态数据集：用来标注数据状态的内置数据集，一般存放在专门的状态主题文件夹
