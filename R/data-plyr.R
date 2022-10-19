## 打开数据集 ----

#' @title 读取数据集
#' @family data-plyr function
#' @export
dp_read <- function(dsName, dataName = "@result") {
  ex <- expression({
    ds_read0(dsName, cacheTopic)
  })
  list(
    "scriptType" = "dp_read",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "dataName" = dataName,
      "dsName" = dsName)
    )
}

#' @title 立即执行数据收集（结束惰性计算）
#' @family data-plyr function
#' @export
dp_collect <- function(dataName = "@result") {
  ex <- expression({
    get(dataName) |> collect()
  })
  list(
    "scriptType" = "dp_collect",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "dataName" = dataName)
  )
}

## 行操作 ----

#' @title 定义数据过滤任务
#' @description
#' 这个任务的目标是通过配置项实现函数[dplyr::filter()]的大部分功能。
#' 
#' 允许为数据集增加多个阈值查询条件，缩小筛查范围。
#' 
#' taskId、column、op、value等参数构造唯一的dp_filter表达式，
#' 这将允许从UI生成或还原该操作。
#' 
#' 只要包括以下逻辑判断：
#' \itemize{
#'  \item >, <, >=, <=, ==, !=
#'  \item %in%, %nin%
#'  \item %regex%, %not-regex%
#'  \item #time# >, #time# <
#'  \item #date# >, #date# <
#' }
#' 
#' @param taskId 任务ID
#' @param column 列名
#' @param op 阈值范围判断符号
#' @param value 阈值
#' @param dataName 内存中的数据框名称，默认为 @result
#' @param taskTopic 任务定义的主题文件夹
#' @family data-plyr function
#' @export
dp_filter <- function(column, op, value = list(NULL), dataName = "@result") {
  ## 校验参数合法性
  if(stringr::str_detect(op, "(>|<|>=|<=|==|!=|%in%|%nin%|%regex%|%not-regex%)", negate = TRUE)) {
    stop("Invalid filter OP: ", op)
  }
  ## 创建任务表达式
  if(op %in% c(">", "<", ">=", "<=", "==", "!=", "%in%")) {
    ex <- expression({
      get(dataName) |>
        filter(do.call(!!sym(op), args = list(!!sym(column), unlist(value)))) |>
        collect()
    })
  } else if(stringr::str_detect(op, "^[@#% ]*time[@#% ]+(>|<|>=|<=|==)[ ]*$")) {
    ex <- expression({
      myop <- stringr::str_replace(op, "[@#%]?time[@#% ]+", "") |> stringr::str_trim()
      data <- get(dataName)
      param <- list()
      x1 <- data[[column]] |> as_datetime(tz = "Asia/Shanghai")
      x2 <- unlist(value) |> as_datetime(tz = "Asia/Shanghai")
      data |>
        filter(do.call(myop, args = list(x1, x2))) |>
        collect()
    })
  } else if(stringr::str_detect(op, "^[@#% ]*date[@#% ]+(>|<|>=|<=|==)[ ]*$")) {
    ex <- expression({
      myop <- stringr::str_replace(op, "[@#%]?date[@#% ]+", "") |> stringr::str_trim()
      data <- get(dataName)
      param <- list()
      x1 <- data[[column]] |> as_date()
      x2 <- unlist(value) |> as_date()
      data |>
        filter(do.call(myop, args = list(x1, x2))) |>
        collect()
    })
  } else if(op %in% c("%nin%")) {
    ## 将 %nin% 转换为可以惰性执行的 %in%
    ex <- expression({
      get(dataName) |>
        filter(!do.call("%in%", args = list(!!sym(column), unlist(value)))) |>
        collect()
    })
  } else if(op %in% c("%regex%", "%not-regex%")) {
    ## 正则表达式需要不能惰性执行，需要提前collect数据
    ex <- expression({
      get(dataName) |> collect() |> filter(do.call(!!sym(op), args = list(!!sym(column), unlist(value))))
    })
  } else {
    stop("<dp_filter> Unknown OP: ", op)
  }
  list(
    "scriptType" = "dp_filter",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "dataName" = dataName,
      "column" = column,
      "op" = op,
      "value" = value))
}

#' @title 头部数据
#' @family data-plyr function
#' @export
dp_head <- function(n = 10, dataName = "@result") {
  ex <- expression({
    get(dataName) |> head(n)
  })
  list(
    "scriptType" = "dp_head",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "dataName" = dataName,
      "n" = n)
  )
}

#' @title 尾部数据
#' @family data-plyr function
#' @export
dp_tail <- function(n = 10, dataName = "@result") {
  ex <- expression({
    get(dataName) |> tail(n)
  })
  list(
    "scriptType" = "dp_tail",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "dataName" = dataName,
      "n" = n)
  )
}

#' @title 按最大值取N条记录
#' @family data-plyr function
#' @export
dp_n_max <- function(orderColumn, n = 10, with_ties = FALSE, dataName = "@result") {
  ex <- expression({
    mydata <- get(dataName) |> collect()
    mydata |> slice_max(order_by = mydata[[orderColumn]], n = n, with_ties = with_ties)
  })
  list(
    "scriptType" = "dp_n_max",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "dataName" = dataName,
      "orderColumn" = orderColumn,
      "n" = n,
      "with_ties" = with_ties)
  )
}

#' @title 按最小值取N条记录
#' @family data-plyr function
#' @export
dp_n_min <- function(orderColumn, n = 10, with_ties = FALSE, dataName = "@result") {
  ex <- expression({
    mydata <- get(dataName) |> collect()
    mydata |> slice_min(order_by = mydata[[orderColumn]], n = n, with_ties = with_ties)
  })
  list(
    "scriptType" = "dp_n_min",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "dataName" = dataName,
      "orderColumn" = orderColumn,
      "n" = n,
      "with_ties" = with_ties)
  )
}

## 列操作----

#' @title 按最小值取N条记录
#' @family data-plyr function
#' @export
dp_select <- function(columns = list(), everything = FALSE,
                      regex = NULL, dataName = "@result") {
  ex <- expression({
    mydata <- get(dataName)
    if(everything) {
      mydata |> select(contains(columns), matches(regex), everything())
    } else {
      mydata |> select(contains(columns), matches(regex))
    }
  })
  list(
    "scriptType" = "dp_n_min",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "dataName" = dataName,
      "columns" = columns |> unlist(),
      "regex" = regex %empty% "^mamahannihuijiachifan$",
      "everything" = everything)
  )
}