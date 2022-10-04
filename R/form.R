#' @title 写入表单数据
#' @description 为队列中待执行的任务保存输入参数
#' @param data 表单数据，一般以列表结构保存
#' @param formId 保障唯一，每个\code{formId}会保存为一个独立文件
#' @param taskType 任务类型
#' @param formTopic 保存表单的存储主题文件夹
#' @param family form function
#' @export
form_write <- function(data, formId, taskType = "common", formTopic = "FORM") {
  get_path(formTopic, taskType, formId) |>
    fs::path_dir() |>
    fs::dir_create()
  data |>
    saveRDS(get_path(formTopic, taskType, paste0(formId, ".rds")))
}

#' @title 读取表单数据
#' @description 
#' 读取队列中待执行的任务的输入参数
#' @param formId 每个批次号保存为一个文件
#' @param taskType 任务类型
#' @param formTopic 保存表单的存储主题文件夹
#' @param family form function
#' @export
form_read <- function(formId, taskType = "common", formTopic = "FORM") {
  path <- get_path(formTopic, taskType, paste0(formId, ".rds"))
  if(fs::file_exists(path)) {
    readRDS(path)
  } else {
    warning("No Form data: ", formId)
    list()
  }
}
