#' @title 初始化导入数据集
#' @description 
#' 导入配置涉及到输入素材、输出结果，以及处理队列和处理脚本等四方面的联系。
#' \itemize{
#'  \item 输入素材一般放置在IMPORT主题文件夹下，由爬虫应用等从外部写入；
#'  \item 输出结果一般保存在CACHE主题文件夹下，与其他指标构建类似；
#'  \item 处理队列时导入输入素材的处理顺序整理，整理结果保存在QUEUE主题文件下；
#'  \item 处理脚本一般保存在TASK主题文件夹下，按照处理队列中的顺序执行脚本。
#' }
#' @param ... 参考函数 \link{ds_init}
#' @param importName 导入素材所在的文件夹名称
#' @param importTopic 导入素材主题文件夹位置
#' @param taskName 任务脚本名称
#' @param taskTopic 任务脚本主题文件夹位置
#' @family dataset-import function
#' @export
ds_import_init <- function(
    ...,
    importName, importTopic = "IMPORT",
    taskName, taskTopic = "TASK") {
  ## 要补充的元数据
  ex <- list(
    "importing" = list(
      "importName" = importName,
      "importTopic" = importTopic,
      "taskName" = taskName,
      "taskTopic" = taskTopic      
    ))
  
  ## 初始化任务文件
  pathTask <- get_path(taskTopic, paste0(taskName, ".R"))
  fs::path_dir(pathTask) |> fs::dir_create()
  fs::file_touch(pathTask)
  
  ## 写入配置文件
  ds_yaml_write(..., ex = ex, type = "import")
}