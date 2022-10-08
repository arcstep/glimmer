#' @title 初始化队列数据集
#' @description  批处理任务的队列
#' @family task function
#' @export
ds_task_queue_init <- function(dsName = "__TASK_QUEUE__", cacheTopic = "CACHE") {
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

#
queue_param_from_yaml <- function(ymlParams) ymlParams |> yaml::yaml.load()
queue_param_to_yaml <- function(params) params |> yaml::as.yaml()

#' @title 构造队列中的一条数据
#' @family task function
#' @export
ds_task_queue_item <- function(taskId, params, taskType = "__TYPE_UNKNOWN__", taskTopic = "CACHE", id = gen_batchNum()) {
  createdAt <- now(tzone = "Asia/Shanghai")
  list(
    "id" = id,
    "taskTopic" = taskTopic,
    "taskType" = taskType,
    "taskId" = taskId,
    "ymlParams" = params,
    "createdAt" = createdAt,
    "year" = as.integer(lubridate::year(createdAt)),
    "month" = as.integer(lubridate::month(createdAt)))
}

#' @title 待执行的队列任务
#' @family task function
#' @export
ds_task_queue_todo <- function(taskType = NULL, dsName = "__TASK_QUEUE__", cacheTopic = "CACHE") {
  all_tasks <- ds_read(dsName = dsName, topic = cacheTopic)
  if(!rlang::is_empty(all_tasks)) {
    (all_tasks |>
       filter(!is.na(runAt)) |>
       filter(taskType %in% taskType) |>
       collect() |>
       distinct(`@batchId`))$`@batchId` |> sort()
  } else {
    c()
  }
}

#' @title 批量执行队列任务
#' @description 
#' 相同批次的任务按优先级大小依次执行；
#' 执行后的结果统一更新到队列。
#' 
#' 如果批任务中部分任务失败，则整体批次任务暂停，
#' 批次中的所有任务信息都不更新。
#' @family task function
#' @export
ds_task_queue_run <- function(batchId, runMode = "in-process", dsName = "__TASK_QUEUE__", cacheTopic = "CACHE") {
  all_tasks <- ds_read(dsName = dsName, topic = cacheTopic) |>
    filter(`@batchId` == batchId) |>
    arrange(desc(runLevel)) |>
    select(id, taskId, taskTopic, taskType, runLevel, ymlParams, createdAt, year, month) |>
    purrr::pmap_df(function(id, taskId, taskTopic, taskType, runLevel, ymlParams, createdAt, year, month) {
      arg <- ymlParams |> queue_param_from_yaml()
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