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
    "dsName" = dt_string(),
    "dataId" = dt_string(), # 转换为字符串显示，多个ID使用#间隔
    "dataTitle" = dt_string(),
    "riskLevel" =  dt_string(),
    "riskTip" = dt_string(),
    "submitAt" = dt_datetime(),
    "doneAt" = dt_datetime(),
    "year" = dt_int(),    # submitAt year
    "month" = dt_int())   # submitAt month
  ds_init(
    dsName = dsName,
    topic = cacheTopic,
    data = sampleData,
    keyColumns = c("dataId", "dsName", "modelName"), # 针对同一份数据集，相同模型名称仅生成一条数据
    partColumns = c("year", "month"),
    type = "__RISK__")
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
                           tagName = "main",
                           riskTip = "-",
                           riskLevel = "L",
                           modelDesc = "-",
                           author = "-",
                           riskDataName = "__RISK_DATA__",
                           cacheTopic = "CACHE",
                           taskTopic = "TASK_DEFINE",
                           scriptsTopic = "TASK_SCRIPTS") {
  modelId = paste(modelName, tagName, sep = "/")
  ## 校验筛查目标的数据集配置
  yml <- ds_yaml(dsName, topic = cacheTopic)
  if(rlang::is_empty(yml$keyColumns)) {
    stop("Risk Model Config Invalid: ", modelId, " / ", "No KeyColumns in dataset ", dsName)
  }
  if(rlang::is_empty(yml$titleColumn)) {
    stop("Risk Model Config Invalid: ", modelId, " / ", "No TitleColumn in dataset ", dsName)
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
                dsName = dsName,
                riskDataName = riskDataName,
                riskTip = riskTip,
                riskLevel = riskLevel)) |>
    task_item_add(expression({dsName |> ds_read(topic = cacheTopic)}) |> as.character(),
                  params = list(
                    "dsName" = dsName,
                    "modelId" = modelId,
                    "riskDataName" = riskDataName,
                    "cacheTopic" = cacheTopic,
                    "taskTopic" = taskTopic,
                    "scriptsTopic" = scriptsTopic),
                  scriptType = "string",
                  taskTopic = taskTopic)
}

#' @title 生成疑点数据
#' @description
#' 将筛查后的疑点数据写入数据集
#' 
#' @family risk function
#' @export
risk_data_build <- function(modelId,
                            riskDataName = "__RISK_DATA__",
                            cacheTopic = "CACHE",
                            taskTopic = "TASK_DEFINE",
                            scriptsTopic = "TASK_SCRIPTS") {
  task_item_add(modelId,
                expression({
                  `@result` |>
                    risk_data_write(dsName,
                                    modelId = modelId,
                                    riskDataName = riskDataName,
                                    cacheTopic = cacheTopic,
                                    taskTopic = taskTopic)
                  }) |> as.character(),
                scriptType = "string",
                taskTopic = taskTopic)
}

#' @title 读取疑点数据
#' @family risk function
#' @export
risk_data_read <- function(todoFlag = TRUE, riskDataName = "__RISK_DATA__", cacheTopic = "CACHE") {
  x <- ds_read0(riskDataName, cacheTopic)
  if(rlang::is_empty(x)) {
    tibble()
  } else {
    x |> collect() |> filter(is.na(doneAt) %in% todoFlag)
  }
}

#' @title 查找风险模型
#' @family risk function
#' @export
risk_model_search <- function(modelMatch = ".*", taskTopic = "TASK_DEFINE") {
  root_path <- get_path(taskTopic)
  if(fs::dir_exists(root_path)) {
    tasks <- fs::dir_ls(root_path, type = "file", all = T, glob = "*.rds", recurse = T)
    if(length(tasks) > 0) {
      tasks |>
        purrr::map_df(function(path) {
          x <- readRDS(path)
          x$itemsCount <- length(x$items[1])
          x$items <- list(as_tibble(x$items))
          names(x$extention) |> purrr::walk(function(item) x[[item]] <<- x$extention[[item]])
          x$online <- x$online %empty% TRUE
          x$extention <- list(x$extention)
          x
        }) |>
        filter(stringr::str_detect(taskId, modelMatch)) |>
        filter(stringr::str_detect(taskType, "__RISK__")) |>
        select(-taskType, -extention) |>
        select(taskId, online, modelName, tagName, dsName, riskLevel, riskTip, itemsCount, everything())
    } else {
      tibble()
    }
  } else {
    tibble()
  }
}

#' @title 清理未处理的疑点数据
#' @description
#' 重新生成模型时，一般需要清理未处理的疑点数据
#' 
#' @family risk function
#' @export
risk_data_clear <- function(modelId,
                            riskDataName = "__RISK_DATA__",
                            cacheTopic = "CACHE",
                            taskTopic = "TASK_DEFINE") {
  task <- task_read(modelId, taskTopic = taskTopic)
  ds_read(riskDataName, topic = cacheTopic) |>
    filter(is.na(doneAt)) |>
    collect() |>
    ds_as_deleted() |>
    ds_append(dsName = riskDataName, topic = cacheTopic)
}

#' @title 写入疑点数据
risk_data_write <- function(d,
                            datasetName,
                            modelId,
                            riskDataName = "__RISK_DATA__",
                            cacheTopic = "CACHE",
                            taskTopic = "TASK_DEFINE") {
  task <- task_read(modelId, taskTopic = taskTopic)
  dsYaml <- ds_yaml(datasetName, cacheTopic)
  submitTime <- now(tzone = "Asia/Shanghai")
  d |>
    select(dsYaml$keyColumns, dsYaml$titleColumn) |>
    collect() |>
    unite("dataId", dsYaml$keyColumns) |>
    unite("dataTitle", dsYaml$titleColumn) |>
    mutate(modelName = task$extention$modelName) |>
    mutate(dsName = datasetName) |>
    mutate(tagName = task$extention$tagName) |>
    mutate(batchNumber = gen_batchNum()) |>
    mutate(riskLevel = task$extention$riskLevel) |>
    mutate(riskTip = task$extention$riskTip) |>
    mutate(submitAt = submitTime) |>
    mutate(year = year(submitTime), month = month(submitTime)) |>
    ds_write(riskDataName, cacheTopic)
}
