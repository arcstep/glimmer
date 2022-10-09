#' @title 创建BUILD任务批处理队列
#' @family build function
#' @export
build_task_queue_create <- function(taskQueue = "__TASK_QUEUE__",
                                    taskTopic = "TASK_DEFINE",
                                    cacheTopic = "CACHE") {
  ## 扫描所有未处理、未忽略、已完成匹配的导入素材文件
  allTasks <- task_all(taskTopic)
  if(!rlang::is_empty(allTasks)) {
    allTasks |>
      filter(taskType == "__BUILD__" & online) |>
      select(taskTopic, taskId) |>
      purrr::pmap_df(function(taskTopic, taskId) {
        task_queue_item(taskId = taskId,
                        taskType = "__BUILD__",
                        taskTopic = cacheTopic)
      })
      ds_append(dsName = taskQueue, topic = cacheTopic)      
  }
}

#' @title 执行构建任务
#' @family build function
#' @export
build_task_queue_run <- function() {
  task_queue_todo(taskType == "__BUILD__") |>
    purrr::walk(function(batchId) task_queue_run(batchId))
}
