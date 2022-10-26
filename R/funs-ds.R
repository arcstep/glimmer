
#' @title 立即执行数据收集（结束惰性计算）
#' @family gali-dataset function
#' @export
ds_collect <- function(d) {
  d |> collect()
}

## 行操作 ----

#' @title 定义数据过滤任务
#' @description
#' 这个任务的目标是通过配置项实现函数[dplyr::filter()]的大部分功能。
#' 
#' 允许为数据集增加多个阈值查询条件，缩小筛查范围。
#' 
#' taskId、column、op、value等参数构造唯一的gali_ds_filter表达式，
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
#' @param column 列名
#' @param op 阈值范围判断符号
#' @param value 阈值
#' @param s_dataName 内存中的数据框名称，默认为 @result
#' @param taskTopic 任务定义的主题文件夹
#' @family gali-dataset function
#' @export
ds_filter <- function(d, column, op, value = list(NULL)) {
  ## 校验参数合法性
  if(stringr::str_detect(op, "(>|<|>=|<=|==|!=|%in%|%nin%|%regex%|%not-regex%)", negate = TRUE)) {
    stop("Invalid filter OP: ", op)
  }

  ## 创建任务表达式
  if(op %in% c(">", "<", ">=", "<=", "==", "!=", "%in%")) {
    d |> filter(do.call(!!sym(op), args = list(!!sym(column), unlist(value))))
  } else if(stringr::str_detect(op, "^[@#% ]*time[@#% ]+(>|<|>=|<=|==)[ ]*$")) {
    myop <- stringr::str_replace(op, "[@#%]?time[@#% ]+", "") |> stringr::str_trim()
    param <- list()
    x1 <- d[[column]] |> as_datetime(tz = "Asia/Shanghai")
    x2 <- unlist(value) |> as_datetime(tz = "Asia/Shanghai")
    d |> filter(do.call(myop, args = list(x1, x2)))
  } else if(stringr::str_detect(op, "^[@#% ]*date[@#% ]+(>|<|>=|<=|==)[ ]*$")) {
    myop <- stringr::str_replace(op, "[@#%]?date[@#% ]+", "") |> stringr::str_trim()
    param <- list()
    x1 <- d[[column]] |> as_date()
    x2 <- unlist(value) |> as_date()
    d |> filter(do.call(myop, args = list(x1, x2)))
  } else if(op %in% c("%nin%")) {
    ## 将 %nin% 转换为可以惰性执行的 %in%
    d |> filter(!do.call("%in%", args = list(!!sym(column), unlist(value))))
  } else if(op %in% c("%regex%", "%not-regex%")) {
    ## 正则表达式需要不能惰性执行，需要提前collect数据
    d |> collect() |>
      filter(do.call(!!sym(op), args = list(!!sym(column), unlist(value))))
  } else {
    stop("<gali_ds_filter> Unknown OP: ", op)
  }
}

#' @title 头部数据
#' @family gali-dataset function
#' @export
ds_head <- function(d, n = 10) {
  d |> head(n)
}

#' @title 尾部数据
#' @family gali-dataset function
#' @export
ds_tail <- function(d, n = 10) {
  d |> tail(n)
}

#' @title 按最大值取N条记录
#' @family gali-dataset function
#' @export
ds_n_max <- function(d, orderColumn, n = 10, with_ties = FALSE) {
  mydata <- d |> collect()
  mydata |>
    slice_max(order_by = mydata[[orderColumn]], n = n, with_ties = with_ties)
}

#' @title 按最小值取N条记录
#' @family gali-dataset function
#' @export
ds_n_min <- function(d, orderColumn, n = 10, with_ties = FALSE) {
  mydata <- d |> collect()
  mydata |>
    slice_min(order_by = mydata[[orderColumn]], n = n, with_ties = with_ties)
}

## 行排序----

#' @title 行排序
#' @family gali-dataset function
#' @export
ds_arrange <- function(d, columns = list(), desc = FALSE, by_group = FALSE) {
  if(desc) {
    d |> arrange(desc(!!!syms(columns)), .by_group = by_group)
  } else {
    d |> arrange(!!!syms(columns), .by_group = by_group)
  }
}

## 列操作----

#' @title 选择列，支持惰性计算
#' @family gali-dataset function
#' @export
ds_select <- function(d, columns = list(), showOthers = FALSE, regex = NULL) {
  d |>
    select(contains(columns |> unlist()),
           matches(regex %empty% "^mamaxiannichifanman$"), if(showOthers) everything() else NULL)
}

#' @title 列改名
#' @family gali-dataset function
#' @export
ds_rename <- function(d, newName, oldName) {
  mydata <- d |> collect()
  names(mydata)[names(mydata) == oldName] <- newName
  mydata
}

#' @title 计数统计
#' @family gali-dataset function
#' @export
ds_count <- function(d, columns = c(), sort = FALSE, name = "n") {
  d |>
    select(columns) |>
    collect() |>
    count(!!!syms(columns), sort = sort, name = name)
}

#' @title 计数统计，并将结果追加到原数据集
#' @family gali-dataset function
#' @export
ds_add_count <- function(d, columns = c(), sort = FALSE, name = "n") {
  d |> collect() |> add_count(!!!syms(columns), sort = sort, name = name)
}