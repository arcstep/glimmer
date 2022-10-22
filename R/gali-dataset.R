## 存取数据集 ----

#' @title 读取数据集
#' @family gali-dataset function
#' @export
gali_read <- function(s_dsName, b_noDeleted = TRUE) {
  ds_read0(s_dsName, noDeleted = b_noDeleted)
}

#' @title 写入数据集
#' @family gali-dataset function
#' @export
gali_write <- function(d = NULL, s_dsName) {
  (d %empty% get(s_OUTPUT)) |>
    collect() |>
    ds_write(s_dsName)
}

#' @title 立即执行数据收集（结束惰性计算）
#' @family gali-dataset function
#' @export
gali_ds_collect <- function(d = NULL) {
  (d %empty% get(s_OUTPUT)) |>
    collect()
}

## 行操作 ----

#' @title 定义数据过滤任务
#' @description
#' 这个任务的目标是通过配置项实现函数[dplyr::filter()]的大部分功能。
#' 
#' 允许为数据集增加多个阈值查询条件，缩小筛查范围。
#' 
#' taskId、s_column、o_name、fv_value等参数构造唯一的gali_ds_filter表达式，
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
#' @param s_column 列名
#' @param o_name 阈值范围判断符号
#' @param fv_value 阈值
#' @param s_dataName 内存中的数据框名称，默认为 @result
#' @param taskTopic 任务定义的主题文件夹
#' @family gali-dataset function
#' @export
gali_ds_filter <- function(d = NULL, s_column, o_name, fv_value = list(NULL)) {
  ## 校验参数合法性
  if(stringr::str_detect(o_name, "(>|<|>=|<=|==|!=|%in%|%nin%|%regex%|%not-regex%)", negate = TRUE)) {
    stop("Invalid filter OP: ", o_name)
  }

  mydata <- d %empty% get(s_OUTPUT)
  ## 创建任务表达式
  if(o_name %in% c(">", "<", ">=", "<=", "==", "!=", "%in%")) {
    mydata |> filter(do.call(!!sym(o_name), args = list(!!sym(s_column), unlist(fv_value))))
  } else if(stringr::str_detect(o_name, "^[@#% ]*time[@#% ]+(>|<|>=|<=|==)[ ]*$")) {
    myop <- stringr::str_replace(o_name, "[@#%]?time[@#% ]+", "") |> stringr::str_trim()
    param <- list()
    x1 <- mydata[[s_column]] |> as_datetime(tz = "Asia/Shanghai")
    x2 <- unlist(fv_value) |> as_datetime(tz = "Asia/Shanghai")
    mydata |> filter(do.call(myop, args = list(x1, x2)))
  } else if(stringr::str_detect(o_name, "^[@#% ]*date[@#% ]+(>|<|>=|<=|==)[ ]*$")) {
    myop <- stringr::str_replace(o_name, "[@#%]?date[@#% ]+", "") |> stringr::str_trim()
    param <- list()
    x1 <- mydata[[s_column]] |> as_date()
    x2 <- unlist(fv_value) |> as_date()
    mydata |> filter(do.call(myop, args = list(x1, x2)))
  } else if(o_name %in% c("%nin%")) {
    ## 将 %nin% 转换为可以惰性执行的 %in%
    mydata |> filter(!do.call("%in%", args = list(!!sym(s_column), unlist(fv_value))))
  } else if(o_name %in% c("%regex%", "%not-regex%")) {
    ## 正则表达式需要不能惰性执行，需要提前collect数据
    mydata |> collect() |>
      filter(do.call(!!sym(o_name), args = list(!!sym(s_column), unlist(fv_value))))
  } else {
    stop("<gali_ds_filter> Unknown OP: ", o_name)
  }
}

#' @title 头部数据
#' @family gali-dataset function
#' @export
gali_ds_head <- function(d = NULL, i_n = 10) {
  d %empty% get(s_OUTPUT) |> head(i_n)
}

#' @title 尾部数据
#' @family gali-dataset function
#' @export
gali_ds_tail <- function(d = NULL, i_n = 10) {
  d %empty% get(s_OUTPUT) |> tail(i_n)
}

#' @title 按最大值取N条记录
#' @family gali-dataset function
#' @export
gali_ds_n_max <- function(d = NULL, s_orderColumn, i_n = 10, b_with_ties = FALSE) {
  mydata <- d %empty% get(s_OUTPUT) |> collect()
  mydata |> slice_max(order_by = mydata[[s_orderColumn]], n = i_n, with_ties = b_with_ties)
}

#' @title 按最小值取N条记录
#' @family gali-dataset function
#' @export
gali_ds_n_min <- function(d = NULL, s_orderColumn, i_n = 10, b_with_ties = FALSE) {
  mydata <- d %empty% get(s_OUTPUT) |> collect()
  mydata |> slice_min(order_by = mydata[[s_orderColumn]], n = i_n, with_ties = b_with_ties)
}

## 行排序----

#' @title 行排序
#' @family gali-dataset function
#' @export
gali_ds_arrange <- function(d = NULL, sv_columns = list(), b_desc = FALSE, b_by_group = FALSE) {
  mydata <- d %empty% get(s_OUTPUT)
  if(b_desc) {
    mydata |> arrange(desc(!!!syms(sv_columns)), .by_group = b_by_group)
  } else {
    mydata |> arrange(!!!syms(sv_columns), .by_group = b_by_group)
  }
}

## 列操作----

#' @title 选择列，支持惰性计算
#' @family gali-dataset function
#' @export
gali_ds_select <- function(d = NULL, sv_columns = list(), b_everything = FALSE, s_regex = NULL) {
  d %empty% get(s_OUTPUT) |>
    select(contains(sv_columns |> unlist()),
           matches(s_regex %empty% "^mamaxiannichifanman$"), if(b_everything) everything() else NULL)
}

#' @title 列改名
#' @family gali-dataset function
#' @export
gali_ds_rename <- function(d = NULL, s_newName, s_oldName) {
  mydata <- d %empty% get(s_OUTPUT) |> collect()
  names(mydata)[names(mydata) == s_oldName] <- s_newName
  mydata
}

#' @title 计数统计
#' @family gali-dataset function
#' @export
gali_ds_count <- function(d = NULL, sv_columns = c(), b_sort = FALSE, s_name = "n") {
  d %empty% get(s_OUTPUT) |>
    select(sv_columns) |>
    collect() |>
    count(!!!syms(sv_columns), sort = b_sort, name = s_name)
}

#' @title 计数统计，并将结果追加到原数据集
#' @family gali-dataset function
#' @export
gali_ds_add_count <- function(d = NULL, sv_columns = c(), b_sort = FALSE, s_name = "n") {
  d %empty% get(s_OUTPUT) |>
    collect() |>
    add_count(!!!syms(sv_columns), sort = b_sort, name = s_name)
}
