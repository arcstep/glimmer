#' @title 为风险模型增加阈值条件
#' @description
#' 允许为数据集增加多个阈值查询条件，缩小筛查范围。
#' 
#' 由modelId、column、op、value等参数构造唯一的dp_filter表达式，
#' 这将允许从UI生成或还原该操作。
#' 
#' @family data-plyr function
#' @export
dp_filter <- function(modelId, column, op, value,
                      taskTopic = "TASK_DEFINE",
                      scriptsTopic = "TASK_SCRIPTS") {
  ## 校验参数合法性
  if(stringr::str_detect(op, "(>|<|>=|<=|==|!=|%in%|%nin%|%regex%|%not-regex%)", negate = TRUE)) {
    stop("Risk Model: ", modelId, " >> Unknown OP: ", op)
  }
  ## 创建任务表达式
  if(op %in% c(">", "<", ">=", "<=", "==", "!=", "%in%")) {
    ex <- expression({
      output |>
        filter(do.call(!!sym(op), args = list(!!sym(column), unlist(value)))) |>
        collect()
    })
  } else if(stringr::str_detect(op, "^[@#% ]*time[@#% ]+(>|<|>=|<=|==)[ ]*$")) {
    ex <- expression({
      myop <- stringr::str_replace(op, "[@#%]?time[@#% ]+", "") |> stringr::str_trim()
      output |>
        filter(do.call(!!sym(myop), args = list(!!sym(column) |> lubridate::as_datetime(tz = "Asia/Shanghai"),
                                                unlist(value) |> lubridate::as_datetime(tz = "Asia/Shanghai")))) |>
        collect()
    })
  } else if(stringr::str_detect(op, "^[@#% ]*date[@#% ]+(>|<|>=|<=|==)[ ]*$")) {
    ex <- expression({
      myop <- stringr::str_replace(op, "[@#%]?date[@#% ]+", "") |> stringr::str_trim()
      output |>
        filter(do.call(!!sym(myop), args = list(!!sym(column) |> lubridate::as_date(tz = "Asia/Shanghai"),
                                                unlist(value) |> lubridate::as_date(tz = "Asia/Shanghai")))) |>
        collect()
    })
  } else if(op %in% c("%nin%")) {
    ## 将 %nin% 转换为可以惰性执行的 %in%
    ex <- expression({
      output |>
        filter(!do.call("%in%", args = list(!!sym(column), unlist(value)))) |>
        collect()
    })
  } else if(op %in% c("%regex%", "%not-regex%")) {
    ## 正则表达式需要不能惰性执行，需要提前collect数据
    ex <- expression({
      output |> collect() |> filter(do.call(!!sym(op), args = list(!!sym(column), unlist(value))))
    })
  } else {
    stop("Risk Model: ", modelId, " >> Unknown OP: ", op)
  }
  task_item_add(modelId,
                ex |> as.character(),
                params = list(
                  "modelId" = modelId,
                  "column" = column,
                  "op" = op,
                  "value" = value),
                scriptType = "filter",
                taskTopic = taskTopic,
                scriptsTopic = scriptsTopic)
}
