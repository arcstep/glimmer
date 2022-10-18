#' @title 初始化风险筛查数据集
#' @description  通过风险模型筛查疑点数据
#' @family risk function
#' @export
risk_data_init <- function(dsName = "__RISK_DATA__", cacheTopic = "CACHE") {
  ## 任务数据样本
  sampleData <- tibble(
    "modelName" = dt_string(),
    "tagName" = dt_string(), # 可使用版本号或多段阈值形成同类风险
    "batchNumber" = dt_string(),
    "dataId" = dt_string(), # 转换为字符串显示，多个ID使用#间隔
    "dataTitle" = dt_string(),
    "riskLevel" =  dt_int(),
    "riskTip" = dt_string(),
    "submitAt" = dt_datetime(),
    "doneAt" = dt_datetime(),
    "year" = dt_int(),    # submitAt year
    "month" = dt_int())   # submitAt month
  ds_init(
    dsName = dsName,
    topic = cacheTopic,
    data = sampleData,
    keyColumns = c("dataId", "modelName", "@from"), # 针对同一份数据集，相同模型名称仅生成一条数据
    partColumns = c("year", "month"),
    type = "__STATE__")
}

#' @title 定义风险模型
#' @description
#' 风险模型属于任务的一种。
#' 
#' 风险模型定义后，主要支持对数据集做过滤查询。
#' @family risk function
#' @export
risk_model_create <- function(dsName,
                           modelName,
                           tagName = "V1",
                           riskTip = "-",
                           riskLevel = "L",
                           modelDesc = "-",
                           author = "-",
                           riskDataName = "__RISK_DATA__",
                           cacheTopic = "CACHE",
                           taskTopic = "TASK_DEFINE",
                           scriptsTopic = "TASK_SCRIPTS") {
  modelId = paste(modelName, tagName, riskLevel, sep = "@")
  ## 校验筛查目标的数据集配置
  yml <- ds_yaml_schema(dsName, topic = cacheTopic)
  if(rlang::is_empty(yml$keyColumns)) {
    stop("Risk Model Config Invalid: ", modelId, " / ", "No KeyColumns in dataset ", item$dataset)
  }
  if(rlang::is_empty(yml$titleColumn)) {
    stop("Risk Model Config Invalid: ", modelId, " / ", "No TitleColumn in dataset ", item$dataset)
  }

  ## 创建针对数据集的风险筛查任务  
  task_create(taskId = modelId,
              taskType = "__RISK__",
              taskTopic = taskTopic,
              desc = modelDesc,
              extention = list(
                modelName = modelName,
                tagName = tagName,
                author = author,
                riskDataName = riskDataName,
                riskTip = riskTip,
                riskLevel = riskLevel)) |>
    task_item_add(expression({dsName |> ds_read(topic = cacheTopic)}),
                  params = list(list("dsName" = dsName, "cacheTopic" = cacheTopic)),
                  scriptType = "expr",
                  taskTopic = taskTopic,
                  scriptsTopic = scriptsTopic)
}

#' @title 为风险模型增加阈值条件
#' @description
#' 允许为数据集增加多个阈值查询条件，缩小筛查范围。
#' 
#' @family risk function
#' @export
risk_filter_add <- function(modelId, column, op, value) {
  ## 校验参数合法性
  if(stringr::str_detect(op, "(>|<|>=|<=|==|!=|%in%|%nin%|%regex%|%not-regex%)", negate = TRUE)) {
    stop("Risk Model: ", modelId, " >> Unknown OP: ", op)
  }
  ## 创建任务表达式
  task_item_add(modelId, expression({
    if(op %in% c(">", "<", ">=", "<=", "==", "!=", "%in%")) {
      output |>
        filter(do.call(!!sym(op), args = list(!!sym(column), unlist(value)))) |>
        collect()
    } else if(stringr::str_detect(op, "^[@#% ]*time[@#% ]+(>|<|>=|<=|==)[ ]*$")) {
      myop <- stringr::str_replace(op, "[@#%]?time[@#% ]+", "") |> stringr::str_trim()
      output |>
        filter(do.call(!!sym(myop), args = list(!!sym(column) |> lubridate::as_datetime(tz = "Asia/Shanghai"),
                                                unlist(value) |> lubridate::as_datetime(tz = "Asia/Shanghai")))) |>
        collect()
    } else if(stringr::str_detect(op, "^[@#% ]*date[@#% ]+(>|<|>=|<=|==)[ ]*$")) {
      myop <- stringr::str_replace(op, "[@#%]?date[@#% ]+", "") |> stringr::str_trim()
      output |>
        filter(do.call(!!sym(myop), args = list(!!sym(column) |> lubridate::as_date(tz = "Asia/Shanghai"),
                                                unlist(value) |> lubridate::as_date(tz = "Asia/Shanghai")))) |>
        collect()
    } else if(op %in% c("%nin%")) {
      ## 将 %nin% 转换为可以惰性执行的 %in%
      output |>
        filter(!do.call("%in%", args = list(!!sym(column), unlist(value)))) |>
        collect()
    } else if(op %in% c("%regex%", "%not-regex%")) {
      ## 正则表达式需要不能惰性执行，需要提前collect数据
      output |> collect() |> filter(do.call(!!sym(op), args = list(!!sym(column), unlist(value))))
    } else {
      stop("Risk Model: ", modelId, " >> Unknown OP: ", op)
    }}),
    params = list(list("modelId" = modelId,
                     "column" = column,
                     "op" = op,
                     "value" = value)),
    scriptType = "filter",
    taskTopic = taskTopic,
    scriptsTopic = scriptsTopic)
}

#' @title 生成疑点数据
#' @description
#' 将筛查后的疑点数据写入数据集
#' 
#' @family risk function
#' @export
risk_generater_add <- function(modelId) {
  task_item_add(modelId, expression({output |> risk_data_write(dsName, modelId)}),
                params = list(list("modelId" = modelId,
                       "column" = column,
                       "op" = op,
                       "value" = value)),
                scriptType = "expr",
                taskTopic = taskTopic,
                scriptsTopic = scriptsTopic)
}

#' @title 清理未处理的疑点数据
#' @description
#' 重新生成模型时，一般需要清理未处理的疑点数据
#' 
#' @family risk function
#' @export
risk_data_clear <- function(modelId,
                            riskName = "__RISK_DATA__",
                            cacheTopic = "CACHE",
                            taskTopic = "TASK_DEFINE") {
  task <- task_read(modelId, taskTopic = taskTopic)
  ds_read(riskName, topic = cacheTopic) |>
    filter(is.na(doneAt)) |>
    collect() |>
    ds_as_deleted() |>
    ds_append(dsName = riskName, topic = cacheTopic)
}

#' @title 写入疑点数据
risk_data_write <- function(d,
                            dsName,
                            modelId,
                            riskName = "__RISK_DATA__",
                            cacheTopic = "CACHE",
                            taskTopic = "TASK_DEFINE") {
  task <- task_read(modelId, taskTopic = taskTopic)
  dsYaml <- ds_yaml_schema(dsName)
  submitTime <- now(tzone = "Asia/Shanghai")
  d |>
    select(dsYaml$keyColumns, dsYaml$titleColumn) |>
    collect() |>
    unite("dataId", dsYaml$keyColumns) |>
    unite("dataTitle", dsYaml$titleColumn) |>
    mutate(modelName = task$extention$modelName) |>
    mutate(tagName = task$extention$tagName) |>
    mutate(batchName = gen_batchNum()) |>
    mutate(riskLevel = task$extention$riskLevel) |>
    mutate(riskTip = task$extention$riskTip) |>
    mutate(submitAt = submitTime) |>
    mutate(year = year(submitTime), month = month(submitTime)) |>
    ds_write(riskName, cacheTopic)
}
