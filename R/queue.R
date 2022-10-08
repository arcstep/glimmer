#' @title 初始化队列数据集
#' @description  批处理任务的队列
#' @family task function
#' @export
queue_dataset_init <- function(dsName = "__TASK_QUEUE__", cacheTopic = "CACHE") {
  ## 任务数据样本
  sampleData <- tibble(
    "id" = gen_batchNum(),
    "taskTopic" = "TASK_DEFINE",
    "taskType" = "__TYPE_IMPORT__",
    "taskId" = "MY_UNIQUE_TASK_NAME", # from task_define
    "runLevel" = 500L,
    "ymlParams" = list("name" = "adi") |> queue_param_to_yaml(),
    "createdAt" = as_datetime("2022-10-01 08:28:15", tz = "Asia/Shanghai"),
    "runAt" = as_datetime("2022-10-01 08:28:15", tz = "Asia/Shanghai"),
    "doneAt" = as_datetime("2022-10-01 08:30:45", tz = "Asia/Shanghai"),
    "year" = 2022L, # createdAt year
    "month" = 10L)  # createdAt month
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

#' @title 待执行的队列任务
#' @family task function
#' @export
queue_batch_todo <- function(dsName = "__TASK_QUEUE__", cacheTopic = "CACHE") {
  all_tasks <- ds_read(dsName = dsName, topic = cacheTopic)
  if(!rlang::is_empty(all_tasks)) {
    (all_tasks |>
      filter(!is.na(runAt)) |>
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
queue_batch_run <- function(batchId, runMode = "in-process", dsName = "__TASK_QUEUE__", cacheTopic = "CACHE") {
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
      runAt <- now()
      do.call("task_run", args = arg)
      doneAt <- now()
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