#' @title 数据过滤器
#' @description
#' 允许为数据集增加多个阈值查询条件，缩小筛查范围。
#' 
#' taskId、column、op、value等参数构造唯一的dp_filter表达式，
#' 这将允许从UI生成或还原该操作。
#' 
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
