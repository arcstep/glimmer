#' @title 初始化队列数据集
#' @description  处理任务的队列
#' @family queue function
#' @export
ds_queue_init <- function(dsName = "__TASK_QUEUE__", topic = "CACHE") {
  ## 主任务记录
  queueSchema <- list(
    "priorLevel", # 优先集
    "batchId",    # 每个批次包含一组任务，处理顺序先按批次号、再按任务名称
    "taskId",     # 任务ID
    "taskName",   # 任务名称
    "taskTopic",  # 任务存储主题
    "formTopic"   # 表单存储主题
  )
  ds_init(dsName, topic, schema = queueSchema)
}

#' @title 写入任务队列
#' @export
queue_write <- function() {}

#' @title 读取任务队列
#' @export
queue_read <- function() {}
