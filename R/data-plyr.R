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
#' @param dataName 内存中的数据框名称，默认为output
#' @param taskTopic 任务定义的主题文件夹
#' @family data-plyr function
#' @export
dp_filter <- function(taskId,
                      column, op, value,
                      dataName = "output",
                      taskTopic = "TASK_DEFINE") {
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
      get(dataName) |>
        filter(do.call(!!sym(myop), args = list(!!sym(column) |> lubridate::as_datetime(tz = "Asia/Shanghai"),
                                                unlist(value) |> lubridate::as_datetime(tz = "Asia/Shanghai")))) |>
        collect()
    })
  } else if(stringr::str_detect(op, "^[@#% ]*date[@#% ]+(>|<|>=|<=|==)[ ]*$")) {
    ex <- expression({
      myop <- stringr::str_replace(op, "[@#%]?date[@#% ]+", "") |> stringr::str_trim()
      get(dataName) |>
        filter(do.call(!!sym(myop), args = list(!!sym(column) |> lubridate::as_date(tz = "Asia/Shanghai"),
                                                unlist(value) |> lubridate::as_date(tz = "Asia/Shanghai")))) |>
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
    stop("Task Error: ", taskId, " >> Unknown OP: ", op)
  }
  task_item_add(taskId,
                ex |> as.character(),
                params = list(
                  "dataName" = dataName,
                  "column" = column,
                  "op" = op,
                  "value" = value),
                scriptType = "dp_filter",
                taskTopic = taskTopic)
}
