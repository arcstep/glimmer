#' @title 初始化队列数据集
#' @description  批处理任务的队列
#' @family queue function
#' @export
task_queue_init <- function(dsName = "__TASK_QUEUE__", cacheTopic = "CACHE") {
  ## 任务数据样本
  sampleData <- tibble(
    "id" = dt_string(),
    "taskTopic" = dt_string(),
    "taskType" = dt_string(),
    "taskId" = dt_string(),
    "runLevel" = dt_int(),
    "ymlParams" = dt_string(),
    "createdAt" = dt_datetime(),
    "runAt" = dt_datetime(),
    "doneAt" = dt_datetime(),
    "year" = dt_int(), # createdAt year
    "month" = dt_int())  # createdAt month
  ds_init(
    dsName = dsName,
    topic = cacheTopic,
    data = sampleData,
    keyColumns = c("id"),
    partColumns = c("taskType", "year", "month"),
    type = "__STATE__")
}

#' @title 从压马路格式提取队列参数
#' @family queue function
#' @export
task_queue_param_from_yaml <- function(ymlParams) ymlParams |> yaml::yaml.load()

#' @title 转换队列参数为yaml格式
#' @family queue function
#' @export
task_queue_param_to_yaml <- function(params) params |> yaml::as.yaml()

#' @title 构造队列中的一条数据
#' @family queue function
#' @export
task_queue_item <- function(taskId, params,
                            taskType = "__TYPE_UNKNOWN__", taskTopic = "CACHE",
                            id = gen_batchNum(), runLevel = 500L) {
  createdAt <- now(tzone = "Asia/Shanghai")
  list(
    "id" = id,
    "runLevel" = runLevel,
    "taskTopic" = taskTopic,
    "taskType" = taskType,
    "taskId" = taskId,
    "ymlParams" = params,
    "createdAt" = createdAt,
    "year" = as.integer(lubridate::year(createdAt)),
    "month" = as.integer(lubridate::month(createdAt)))
}

#' @title 待执行的队列任务
#' @family queue function
#' @export
task_queue_todo <- function(taskTypes = c("__IMPORT__", "__BUILD__", "__RISK__", "__SUMMARISE__"),
                            dsName = "__TASK_QUEUE__",
                            cacheTopic = "CACHE") {
  all_tasks <- ds_read(dsName = dsName, topic = cacheTopic) |> collect()
  if(!rlang::is_empty(all_tasks)) {
    all_tasks |>
      filter(is.na(runAt)) |>
      filter(taskType %in% taskTypes) |>
      arrange(`@batchId`)
  } else {
    tibble()
  }
}

#' @title 所有队列任务
#' @family queue function
#' @export
task_queue_all <- function(dsName = "__TASK_QUEUE__", cacheTopic = "CACHE") {
  ds_read(dsName = dsName, topic = cacheTopic) |> collect()
}

#' @title 批量执行队列任务
#' @description 
#' 按批次执行队列内的任务。
#' 
#' 相同批次的任务按优先级大小依次执行；
#' 执行结果统一更新到队列。
#' 
#' 如果批任务中部分任务失败，则整体批次任务暂停，
#' 批次中的所有任务信息都不更新。
#' @family queue function
#' @export
task_queue_run <- function(dsQueue,
                           dsName = "__TASK_QUEUE__",
                           cacheTopic = "CACHE",
                           runMode = "in-process") {
  dsQueue |>
    arrange(desc(runLevel)) |>
    select(id, taskId, taskTopic, taskType, runLevel, ymlParams, createdAt, year, month) |>
    purrr::pmap_df(function(id, taskId, taskTopic, taskType, runLevel, ymlParams, createdAt, year, month) {
      arg <- ymlParams |> task_queue_param_from_yaml()
      arg$taskId <- taskId
      arg$taskTopic <- taskTopic
      arg$runMode <- runMode
      ## 执行任务
      runAt <- now(tzone = "Asia/Shanghai")
      do.call("task_run", args = arg)
      doneAt <- now(tzone = "Asia/Shanghai")
      ## 更新队列状态
      list(
        "id" = id,
        "taskTopic" = taskTopic,
        "taskType" = taskType,
        "taskId" = taskId,
        "runLevel" = runLevel,
        "ymlParams" = ymlParams,
        "createdAt" = createdAt,
        "runAt" = runAt,
        "doneAt" = doneAt,
        "year" = year,
        "month" = month)
    }) |> ds_append(dsName = dsName, topic = cacheTopic)
}
