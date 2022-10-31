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
    "todo" = dt_bool(),
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
task_risk_model_create <- function(dsName,
                           modelName,
                           tagName = "main",
                           riskTip = "-",
                           riskLevel = "L",
                           modelDesc = "-",
                           author = "-",
                           force = FALSE,
                           riskDataName = "__RISK_DATA__",
                           cacheTopic = "CACHE",
                           taskTopic = "TASK_DEFINE",
                           scriptsTopic = "TASK_SCRIPTS") {
  modelId = paste(modelName, tagName, sep = "/")
  ## 校验筛查目标的数据集配置
  yml <- ds_yaml(dsName, topic = cacheTopic)
  if(is_empty(yml$keyColumns)) {
    stop("Risk Model Config Invalid: ", modelId, " / ", "No KeyColumns in dataset ", dsName)
  }
  if(is_empty(yml$titleColumn)) {
    stop("Risk Model Config Invalid: ", modelId, " / ", "No TitleColumn in dataset ", dsName)
  }

  ## 创建针对数据集的风险筛查任务  
  task_create(taskId = modelId,
              taskType = "__RISK__",
              taskTopic = taskTopic,
              desc = modelDesc,
              force = force,
              extention = list(
                modelName = modelName,
                tagName = tagName,
                author = author,
                dsName = dsName,
                cacheTopic = cacheTopic,
                riskDataName = riskDataName,
                riskTip = riskTip,
                riskLevel = riskLevel)) |>
    ## 从任务运行环境中自定提取taskId并映射为@modelId
    script_expr_add(expression({`@task`$taskId}), outputAssign = "@modelId") |>
    ## 自动读取dsName
    script_func_add("ds_read0", params = list("dsName" = dsName))
}

#' @title 读取疑点数据
#' @family risk function
#' @export
risk_data_read <- function(todoFlag = TRUE, riskDataName = "__RISK_DATA__", cacheTopic = "CACHE") {
  x <- ds_read0(riskDataName, cacheTopic)
  if(is_empty(x)) {
    tibble()
  } else {
    x |> collect() |> filter(todo %in% todoFlag)
  }
}

#' @title 查找风险模型
#' @family risk function
#' @export
task_risk_model_search <- function(modelMatch = ".*", taskTopic = "TASK_DEFINE") {
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
  allData <- risk_data_read(TRUE, riskDataName, cacheTopic)
  if(!is_empty(allData)) {
    allData |>
      collect() |>
      ds_as_deleted() |>
      ds_append(dsName = riskDataName, topic = cacheTopic)
  }
}

#' @title 为疑点数据设置todo标记
#' @family risk function
#' @export
risk_data_set_todo <- function(d,
                              b_todoFlag = TRUE,
                              riskDataName = "__RISK_DATA__",
                              cacheTopic = "CACHE") {
  risk_data_read(c(TRUE, FALSE), riskDataName, cacheTopic) |>
    semi_join(d |> collect(), by = c("modelName", "dsName", "dataId")) |>
    mutate(todo = b_todoFlag) |>
    ds_write("__RISK_DATA__")
}

#' @title 为疑点数据设置已完成标记
#' @family risk function
#' @export
risk_data_set_done <- function(d,
                           t_doneAt = now(),
                           riskDataName = "__RISK_DATA__",
                           cacheTopic = "CACHE") {
  risk_data_read(TRUE, riskDataName, cacheTopic) |>
    semi_join(d |> collect(), by = c("modelName", "dsName", "dataId")) |>
    mutate(todo = FALSE, doneAt = t_doneAt) |>
    ds_write("__RISK_DATA__")
}

#' @title 根据风险模型生成并写入疑点数据
#' @description 
#' 如果疑点数据已经生成，但未处理，则先清理这些数据
#' 
#' @param d 经过疑点筛查的数据集
#' @param modelId 风险模型Id，一般被提前配置在任务执行环境中，并做映射
#' @family risk function
#' @export
risk_data_write <- function(d, modelId) {
  ## 根据任务ID获取数据
  task <- task_read(modelId)
  if(task$taskType != "__RISK__") {
    stop("Not Risk Model Task: ", modelId, "!!")
  }
  datasetName <- task$extention$dsName
  dsYaml <- ds_yaml(datasetName)
  submitTime <- now(tzone = "Asia/Shanghai")
  ## 清理未处理完的疑点数据
  risk_data_clear(modelId,
                  riskDataName = task$extention$riskDataName,
                  cacheTopic = task$extention$cacheTopic,
                  taskTopic = task$taskTopic)
  ## 潜在疑点数据
  d0 <- d |>
    select(dsYaml$keyColumns, dsYaml$titleColumn) |>
    collect() |>
    unite("dataId", dsYaml$keyColumns) |>
    mutate(dsName = datasetName) |>
    mutate(modelName = task$extention$modelName)
  ## 已处理数据
  allRiskData <- risk_data_read(FALSE, task$extention$riskDataName, task$extention$cacheTopic)
  if(!is_empty(allRiskData)) {
    toWrite <- d0 |> anti_join(
      allRiskData |> filter(!todo) |> select(modelName, dsName, dataId) |> collect(),
      by = c("modelName", "dsName", "dataId"))
  } else {
    toWrite <- d0
  }
  ## 生成疑点数据
  toWrite |>
    unite("dataTitle", dsYaml$titleColumn) |>
    mutate(tagName = task$extention$tagName) |>
    mutate(batchNumber = gen_batchNum()) |>
    mutate(riskLevel = task$extention$riskLevel) |>
    mutate(riskTip = task$extention$riskTip) |>
    mutate(submitAt = submitTime) |>
    mutate(todo = TRUE) |>
    mutate(year = year(submitTime), month = month(submitTime)) |>
    ds_write(task$extention$riskDataName, topic = task$extention$cacheTopic)
}
