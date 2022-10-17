#' @title 初始化队列数据集
#' @description  批处理任务的队列
#' @family queue function
#' @export
task_queue_init <- function(dsName = "__TASK_QUEUE__", cacheTopic = "CACHE") {
  ## 任务数据样本
  sampleData <- tibble(
    "id" = dt_string(),
    "taskTopic" = dt_string(),
    "taskId" = dt_string(),
    "ymlParams" = dt_string(),
    "runAt" = dt_datetime(),
    "doneAt" = dt_datetime(),
    "year" = dt_int(), # createdAt year
    "month" = dt_int())  # createdAt month
  ds_init(
    dsName = dsName,
    topic = cacheTopic,
    data = sampleData,
    keyColumns = c("id"),
    partColumns = c("year", "month"),
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
task_queue_item <- function(taskId, yamlParams = "[]\n", id = gen_batchNum(),
                              taskTopic = "TASK_DEFINE", cacheTopic = "CACHE") {
  runAt <- now(tzone = "Asia/Shanghai")
  tibble(
    "id" = id,
    "taskTopic" = taskTopic,
    "taskId" = taskId,
    "ymlParams" = yamlParams,
    "runAt" = runAt,
    "year" = as.integer(lubridate::year(runAt)),
    "month" = as.integer(lubridate::month(runAt)))
}

#' @title 所有队列任务
#' @family queue function
#' @export
task_queue_search <- function(done = FALSE, taskMatch = ".*", dsName = "__TASK_QUEUE__", cacheTopic = "CACHE") {
  all <- ds_read(dsName = dsName, topic = cacheTopic) |>
    filter((!is.na(doneAt)) %in% done) |>
    collect()
  if(!rlang::is_empty(all)) {
    all |> filter(stringr::str_detect(taskId, taskMatch)) |>
      arrange(desc(runAt))
  }
}

