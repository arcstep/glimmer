#' @title 初始化队列数据集
#' @description  处理任务的队列
#' @family task-import function
#' @export
task_import_init <- function(dsName = "__IMPORT_TASK__", cacheTopic = "CACHE") {
  ## 主任务记录
  sampleData <- tibble(
    "taskTopic" = "TASK",      # 任务存储文件夹
    "importTopic" = "IMPORT",  # 导入存储文件夹
    "batchFolder" = "schedule_2022_10_01", # 导入素材文件夹，用来匹配素材任务,
    "taskFolder" = "联系人清单", # 导入素材文件夹名称，与taskName对应
    "submitAt" = lubridate::as_datetime("2022-10-01 01:10:15"),
    "runAt" = lubridate::as_datetime("2022-10-01 08:28:15"),
    "doneAt" = lubridate::as_datetime("2022-10-01 08:30:45"),
    "year" = 2022L, # submitAt year
    "month" = 10L)  # submitAt month
  ds_init(
    dsName = dsName,
    topic = cacheTopic,
    data = sampleData,
    keyColumns = c("taskFolder"),
    type = "__IMPORT_TASK__")
}

#' @title 创建导入任务的目录结构和脚本文件
#' @description
#' 约定taskFolder与导入素材的目录结构对应。
#' 
#' 当taskFolder与导入素材匹配后，就生成一个任务，
#' 这将会执行taskFolder内所有R脚本文件（包括子文件夹）。
#' @family task-import function
#' @export
task_import_define <- function(taskFolder, taskTopic = "TASK") {
  pathTask <- get_path(taskTopic, "__IMPORT_TASK__", taskFolder, paste0(taskFolder, ".R"))
  fs::path_dir(pathTask) |> fs::dir_create()
  fs::file_touch(pathTask)
  message("R Task File: ", pathTask)
}

#' @title 扫描需要导入的新任务
#' @family task-import function
#' @export
task_import_scan <- function(dsName = "__IMPORT_TASK__", cacheTopic = "CACHE", importTopic = "IMPORT", taskTopic = "TASK") {
  ## 加载任务队列
  taskQueue <- ds_read(dsName) |> collect()
  ## 扫描素材文件夹
  batchFolder <- get_path(importTopic) |> fs::dir_ls(type = "dir", recurse = F)
  if(rlang::is_empty(taskQueue)) {
    toImport <- batchFolder
  } else {
    toImport <- batchFolder[batchFolder %nin% taskQueue$batchFolder]
  }
  ## 扫描任务脚本文件夹
  if(!rlang::is_empty(toImport)) {
    ## 提取所有素材文件
    toImportFiles <- toImport |> fs::dir_ls(recurse = TRUE, type = "file")
    if(!rlang::is_empty(toImportFiles)) {
      ## 加载准备好的脚本任务清单
      pathRoot <- get_path(taskTopic, "__IMPORT_TASK__")
      allTask <- pathRoot |> fs::dir_ls(recurse = TRUE, type = "dir")
      ## 生成任务
    } else {
      warning("No IMPORT Files prepared!!!")
    }
  } else {
    warning("No IMPORT Folder prepared!!!")
  }
}

#' @title 处理导入任务
#' @family task-import function
#' @export
task_import_run <- function() {}
