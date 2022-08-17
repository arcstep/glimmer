# 一个高效的R数据处理框架

这个**R**语言工具包，是为了方便管理R数据处理的任务流。

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
remotes::install_github("arcstep/glimmer")
```

### 2、使用

使用**glimmer**包之前，应当根据自己的实际情况配置主题文件夹。

```{r}
library(glimmer)
set_topic("STATE", "~/glimmer/STATE")
set_topic("IMPORT", "~/glimmer/IMPORT")
set_topic("CACHE", "~/glimmer/CACHE")
set_topic("TASK/IMPORT", "~/glimmer/TASK/IMPORT")
set_topic("TASK/BUILD", "~/glimmer/TASK/BUILD")
set_topic("RISKMODEL", "~/glimmer/RISKMODEL")
```

## 三、使用指南：批量执行加工任务

使用**glimmer**包的数据处理框架，主要是两个步骤：

（1）将数据处理脚本保存在**TASK/BUILD**文件夹中

（2）执行批处理，然在**CACHE**文件夹中查看处理结果

下面是数据处理示例。

### 1、准备一个处理脚本（在 TASK/BUILD 中执行）

准备两个最简单的任务处理脚本。 例如：

```{r}
## 1.R
tibble(a = 1:10) |> ds_write("this_is_my_dataset")
```

```{r}
## 2.R
tibble(a = 1:10) |> ds_write("this_is_my_dataset_2")
```

将这个文件命名为`1.R`和`2.R`，都保存在 `~/glimmer/BUILD` 文件夹中。

### 2、执行任务（结果保存在 CACHE）

执行**run_task**函数就可以批量执行上面的脚本。

```{r}
task_run(taskTopic = "TASK/BUILD")
```

可以使用`ds_read("this_is_my_dataset") |> collect()`查看刚刚添加过的数据集。

## 四、使用指南：导入任务

使用**glimmer**包导入数据主要分为三个步骤：

（1）将导入数据保存在**IMPORT**文件夹中

（2）将处理脚本保存在**TASK/IMPORT**文件夹中

（3）执行处理批处理脚本，然后在**CACHE**文件夹中查看处理结果

下面是导入数据的示例。

### 1、准备导入文件（从 IMPORT 导入）

我们要在`~/glimmer/IMPORT文件夹`中准备一个 **CSV** 文件：

```{csv}
a, b,
1, 2,
3, 4
```

接下来，将其保存在`~/glimmer/IMPORT/import1/mycsv/1.csv`的位置。

请注意，文件名之前有两层文件夹`task1`和`mycsv`。

**glimmer**包通过约定替代接口配置。

上面保存导入数据时已经使用了约定：用`import1`表示**导入任务**，用`mycsv`表示要导入的**数据集**名称。

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

**ds_import** 函数是很有用的帮助函数，上面是很好的使用范例。

### 3、执行任务（结果保存在 CACHE）

执行使用**import_todo**函数就可以批量执行脚本，并导入前面准备好的数据。

```{r}
import_todo()
```

这一命令是增量运行的，即使运行多次，之前准备的脚本也只会被运行一次。

使用**import_redo**函数可以再次处理`task1`文件夹的数据，例如`import_redo("task1")`，即使`task1`已经被处理过。

使用`ds_read("my_dataset") |> collect()`可以查看刚刚导入的数据集。

## 五、风险模型

风险模型是筛查数据的一种方式。 这里采用了较为简单的约定来配置风险模型，并使用YAML格式存储在主题文件夹中。

### 1、配置风险模型

典型的风险模型配置，例如

``` yaml
modelName: 鸾尾花/萼片大
dataset: iris
riskTip: 够大
level: L
filter:
- column: Sepal.Length
  op: '>'
  value: 6.0
- column: Sepal.Width
  op: '>'
  value: 3.0
modelGroup: 鸾尾花/萼片大
modelDesc: '-'
author: '-'
online: yes
createdAt: 2022-08-17 15:06:35
lastModified: 2022-08-17 15:06:35
```

其中：

-   modelName 模型名称
-   dataset 要筛查的目标数据集名称
-   riskTip 风险提示描述
-   level 风险级别：L低，M中，H高
-   filter 筛查条件
    -   column：条件字段
    -   比较方法：可以是大于、小于等符号
    -   value：阈值范围
-   modelGroup 模型组名称
-   modelDesc 模型描述（可选）
-   author 作者（可选）
-   online 是否启用，如果设置为no，则该模型不会被执行
-   createdAt 创建时间（自动生成）
-   lastModified 最后修改时间（自动生成）

约定的规则是这样： 在你设定 **RISKMODEL** 主题文件夹位置后，主题文件夹之后的路径去除**.yml**，就是你的风险模型名称。例如，你可以将该文件保存在`RISKMODEL/鸾尾花/萼片大.yml`，风险模型名称就是**鸾尾花/萼片大**。

### 2、执行风险模型

可以使用**risk_model_run**函数执行风险模型：

``` r
risk_model_run(modelName = "鸾尾花/萼片大", batchNumber = 1)
```

这会将疑点数据补充到疑点数据集中。 通过 **risk_data_read** 函数可以查看疑点数据集。

该数据集拥有约定好的数据结构。

``` yaml
dataId: string
dataTitle: string
value: string
riskLevel: string
riskTip: string
modelName: string
batchNumber: double
runAt: timestamp
lastModifiedAt: timestamp
flag: string
dataset: string
modelGroup: string
```

## 六、关键概念

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
