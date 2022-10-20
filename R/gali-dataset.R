## 打开数据集 ----

#' @title 读取数据集
#' @family data-plyr function
#' @export
gali_read <- function(s_dsName, s_dataName = "@result") {
  ex <- expression({
    ds_read0(s_dsName, cacheTopic)
  })
  list(
    "scriptType" = "gali_read",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "s_dataName" = s_dataName,
      "s_dsName" = s_dsName)
    )
}

#' @title 立即执行数据收集（结束惰性计算）
#' @family data-plyr function
#' @export
gali_collect <- function(s_dataName = "@result") {
  ex <- expression({
    get(s_dataName) |> collect()
  })
  list(
    "scriptType" = "gali_collect",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "s_dataName" = s_dataName)
  )
}

## 行操作 ----

#' @title 定义数据过滤任务
#' @description
#' 这个任务的目标是通过配置项实现函数[dplyr::filter()]的大部分功能。
#' 
#' 允许为数据集增加多个阈值查询条件，缩小筛查范围。
#' 
#' taskId、s_column、op_name、fv_value等参数构造唯一的gali_filter表达式，
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
#' @param op_name 阈值范围判断符号
#' @param fv_value 阈值
#' @param s_dataName 内存中的数据框名称，默认为 @result
#' @param taskTopic 任务定义的主题文件夹
#' @family data-plyr function
#' @export
gali_filter <- function(s_column, op_name, fv_value = list(NULL), s_dataName = "@result") {
  ## 校验参数合法性
  if(stringr::str_detect(op_name, "(>|<|>=|<=|==|!=|%in%|%nin%|%regex%|%not-regex%)", negate = TRUE)) {
    stop("Invalid filter OP: ", op_name)
  }
  ## 创建任务表达式
  if(op_name %in% c(">", "<", ">=", "<=", "==", "!=", "%in%")) {
    ex <- expression({
      get(s_dataName) |>
        filter(do.call(!!sym(op_name), args = list(!!sym(s_column), unlist(fv_value)))) |>
        collect()
    })
  } else if(stringr::str_detect(op_name, "^[@#% ]*time[@#% ]+(>|<|>=|<=|==)[ ]*$")) {
    ex <- expression({
      myop <- stringr::str_replace(op_name, "[@#%]?time[@#% ]+", "") |> stringr::str_trim()
      data <- get(s_dataName)
      param <- list()
      x1 <- data[[s_column]] |> as_datetime(tz = "Asia/Shanghai")
      x2 <- unlist(fv_value) |> as_datetime(tz = "Asia/Shanghai")
      data |>
        filter(do.call(myop, args = list(x1, x2))) |>
        collect()
    })
  } else if(stringr::str_detect(op_name, "^[@#% ]*date[@#% ]+(>|<|>=|<=|==)[ ]*$")) {
    ex <- expression({
      myop <- stringr::str_replace(op_name, "[@#%]?date[@#% ]+", "") |> stringr::str_trim()
      data <- get(s_dataName)
      param <- list()
      x1 <- data[[s_column]] |> as_date()
      x2 <- unlist(fv_value) |> as_date()
      data |>
        filter(do.call(myop, args = list(x1, x2))) |>
        collect()
    })
  } else if(op_name %in% c("%nin%")) {
    ## 将 %nin% 转换为可以惰性执行的 %in%
    ex <- expression({
      get(s_dataName) |>
        filter(!do.call("%in%", args = list(!!sym(s_column), unlist(fv_value)))) |>
        collect()
    })
  } else if(op_name %in% c("%regex%", "%not-regex%")) {
    ## 正则表达式需要不能惰性执行，需要提前collect数据
    ex <- expression({
      get(s_dataName) |> collect() |> filter(do.call(!!sym(op_name), args = list(!!sym(s_column), unlist(fv_value))))
    })
  } else {
    stop("<gali_filter> Unknown OP: ", op_name)
  }
  list(
    "scriptType" = "gali_filter",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "s_dataName" = s_dataName,
      "s_column" = s_column,
      "op_name" = op_name,
      "fv_value" = fv_value))
}

#' @title 头部数据
#' @family data-plyr function
#' @export
gali_head <- function(i_n = 10, s_dataName = "@result") {
  ex <- expression({
    get(s_dataName) |> head(i_n)
  })
  list(
    "scriptType" = "gali_head",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "s_dataName" = s_dataName,
      "i_n" = i_n)
  )
}

#' @title 尾部数据
#' @family data-plyr function
#' @export
gali_tail <- function(i_n = 10, s_dataName = "@result") {
  ex <- expression({
    get(s_dataName) |> tail(i_n)
  })
  list(
    "scriptType" = "gali_tail",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "s_dataName" = s_dataName,
      "i_n" = i_n)
  )
}

#' @title 按最大值取N条记录
#' @family data-plyr function
#' @export
gali_n_max <- function(s_orderColumn, i_n = 10, b_with_ties = FALSE, s_dataName = "@result") {
  ex <- expression({
    mydata <- get(s_dataName) |> collect()
    mydata |> slice_max(order_by = mydata[[s_orderColumn]], n = i_n, with_ties = b_with_ties)
  })
  list(
    "scriptType" = "gali_n_max",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "s_dataName" = s_dataName,
      "s_orderColumn" = s_orderColumn,
      "i_n" = i_n,
      "b_with_ties" = b_with_ties)
  )
}

#' @title 按最小值取N条记录
#' @family data-plyr function
#' @export
gali_n_min <- function(s_orderColumn, i_n = 10, b_with_ties = FALSE, s_dataName = "@result") {
  ex <- expression({
    mydata <- get(s_dataName) |> collect()
    mydata |> slice_min(order_by = mydata[[s_orderColumn]], n = i_n, with_ties = b_with_ties)
  })
  list(
    "scriptType" = "gali_n_min",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "s_dataName" = s_dataName,
      "s_orderColumn" = s_orderColumn,
      "i_n" = i_n,
      "b_with_ties" = b_with_ties)
  )
}

## 行排序----

#' @title 行排序
#' @family data-plyr function
#' @export
gali_arrange <- function(sv_columns = list(),
                       b_desc = FALSE, b_by_group = FALSE, s_dataName = "@result") {
  ex <- expression({
    mydata <- get(s_dataName)
    if(b_desc) {
      mydata |> arrange(desc(!!!syms(sv_columns)), .by_group = b_by_group)
    } else {
      mydata |> arrange(!!!syms(sv_columns), .by_group = b_by_group)
    }
  })
  list(
    "scriptType" = "gali_n_min",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "s_dataName" = s_dataName,
      "sv_columns" = sv_columns,
      "b_desc" = b_desc,
      "b_by_group" = b_by_group)
  )
}

## 列操作----

#' @title 选择列，支持惰性计算
#' @family data-plyr function
#' @export
gali_select <- function(sv_columns = list(), b_everything = FALSE,
                      s_regex = NULL, s_dataName = "@result") {
  ex <- expression({
    mydata <- get(s_dataName)
    if(b_everything) {
      mydata |> select(contains(sv_columns), matches(s_regex), everything())
    } else {
      mydata |> select(contains(sv_columns), matches(s_regex))
    }
  })
  list(
    "scriptType" = "gali_n_min",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "s_dataName" = s_dataName,
      "sv_columns" = sv_columns |> unlist(),
      "s_regex" = s_regex %empty% "^mamahannihuijiachifan$",
      "b_everything" = b_everything)
  )
}

#' @title 列改名
#' @family data-plyr function
#' @export
gali_rename <- function(s_newName, s_oldName, s_dataName = "@result") {
  ex <- expression({
    get(s_dataName) |> rename({{s_newName}} := s_oldName)
  })
  list(
    "scriptType" = "gali_n_min",
    "taskScript" = ex |> as.character(),
    "params" = list(
      "s_dataName" = s_dataName,
      "s_newName" = s_newName,
      "s_oldName" = s_oldName)
  )
}