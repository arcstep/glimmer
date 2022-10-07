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
    partColumns = c("year", "month"),
    type = "__QUEUE__")
}

#
queue_param_from_yaml <- function(ymlParams) ymlParams |> yaml::yaml.load()
queue_param_to_yaml <- function(params) params |> yaml::as.yaml()

#' @title 批量执行队列任务
#' @family task function
#' @export
queue_run <- function(dsName = "__TASK_QUEUE__", cacheTopic = "CACHE") {
  all_tasks <- ds_read(dsName = dsName, topic = cacheTopic)
  if(!rlang::is_empty(all_tasks)) {
    all_tasks |>
      filter(!is.na(runAt)) |>
      collect() |>
      arrange(`@batchId`) |>
      nest(`@batchId`) |>
      purrr::pwalk(function(`@batchId`, data) {
        data |> arrange(desc(runLevel)) |>
          select(taskId, taskTopic, ymlParams) |>
          purrr::pmap_df(function(taskId, taskTopic, ymlParams) {
            arg <- ymlParams |> queue_param_from_yaml()
            arg$taskId <- taskId
            arg$taskTopic <- taskTopic
            arg$runMode <- "in-process"
            ## 执行任务
            do.call("task_run", args = arg)
            ## 更新队列状态
          })
      })
  }
}