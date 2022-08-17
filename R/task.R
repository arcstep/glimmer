## 保存任务管理目录
TASK.ENV <- new.env(hash = TRUE)

#' @title 获取配置
#' @param topic 任务主题
#' @family task functions
#' @export
get_topic <- function(topic) {
  get(topic, envir = TASK.ENV)
}

#' @title 获得所有主题
#' @family task functions
#' @export
get_topics <- function() {
  ls(envir = TASK.ENV)
}

#' @title 根据配置构造数据路径
#' @param topic 任务主题
#' @family task functions
#' @export
get_path <- function(topic, ...) {
  p <- list(...)
  fs::path_join(c(get_topic(topic), unlist(p)))
}

#' @title 设置主题目录
#' @param topic 任务主题
#' @param path 文件夹位置
#' @family task functions
#' @export
set_topic <- function(topic, path) {
  assign(topic, path, envir = TASK.ENV)
}

#' @title 未曾处理过的任务文件夹
#' @description 从状态库中比对，哪些任务尚未处理过，然后自动执行
#' @details 
#' 脚本主要有两类，一是需要任务文件夹输入的，二是不需要任务文件夹的
#' 
#' 第一种情况，典型的是定时导入任务，这很可能是多个任务文件夹
#' 
#' 首先，要循环检查所有任务文件夹，
#' 要针对每一个任务文件夹执行脚本，
#' 直到循环完所有任务文件夹为止。
#' 
#' 其次，脚本任务也可能包含很多子任务，
#' 需要针对每个任务文件夹，按顺序执行所有任务。
#' 这些任务可能是各自逐项检查任务文件夹中的每个子文件夹是否需要处理。
#' 
#' 第二种情况，典型的是风险指标加工，这可能不需要指定任何任务文件夹，
#' 而仅仅是根据已经更新的数据集，做相对复杂的分析和预测，形成疑点数据
#' 
#' @family task functions
#' @export
import_todo <- function(importTopic = "IMPORT", taskTopic = "TASK/IMPORT", taskFolder = "") {
  batchNum <- lubridate::now() |> as.integer()
  tasks <- find_import_todo(importTopic)
  s <- state_read("__IMPORTED_FOLDER__")
  if(!rlang::is_empty(s)) {
    d <- s |> select(importFolder) |> collect()
    tasks[tasks %nin% d$importFolder] |> batch_tasks(taskTopic, taskFolder, batchNum)
  } else {
    tasks |> batch_tasks(taskTopic, taskFolder, batchNum)
  }
}

#' @title 手工指定要处理的任务文件夹
#' @family task functions
#' @export
import_redo <- function(todo = c(), importTopic = "IMPORT", taskTopic = "TASK/IMPORT", taskFolder = "") {
  batchNum <- lubridate::now() |> as.integer()
  import_folders_todo <- find_import_todo(importTopic)
  import_folders_todo[import_folders_todo %in% todo] |>  batch_tasks(taskTopic, taskFolder, batchNum)
}

# 罗列导入主题下所有导入任务文件夹
# 如果希望划分子文件夹管理导入内容（例如按年、月），可以将子文件夹作为导入主题
find_import_todo <- function(importTopic) {
  path = get_path(importTopic)
  fs::dir_ls(path, type = "directory", recurse = FALSE) |>
    sort() |>
    fs::path_file()
}

# 枚举任务文件夹
batch_tasks <- function(importFolders, taskTopic, taskFolder, batchNum) {
  message(length(importFolders), " task folders todo.")
  importFolders |> purrr::walk(function(item) {
    set_topic("__IMPORTING_FOLDER__", item)
    message("SCAN IMPORT FOLDER：", item)
    task_run(taskTopic, taskFolder, batchNum)
    set_topic("__IMPORTING_FOLDER__", "-")
    state_write("__IMPORTED_FOLDER__", tibble(
      "batchNum" = batchNum,
      "importFolder" = item,
      "status" = "DONE",
      "taskTopic" = taskTopic,
      "taskFolder" = taskFolder
    ))
  })
}

#' @title 执行目标路径下的任务脚本
#' @description 应当按照脚本顺序执行
#' @param taskTopic 脚本文件夹主题
#' @param taskFolder 执行脚本文件的目录
#' @family task functions
#' @export
task_run <- function(
    taskTopic = "TASK/BUILD",
    taskFolder = "",
    batchNum = lubridate::now() |> as.integer()) {
  task_files(taskTopic, taskFolder) |> purrr::pwalk(function(name, path) {
    message("RUN TASK SCRIPT：", name)
    beginTime <- lubridate::now(tz = "Asia/Shanghai")
    # 执行脚本
    source(path)
    used <- lubridate::now(tz = "Asia/Shanghai") - beginTime
    msg <- paste0("TASK USED：", as.character.Date(used))
    message(msg)
    # 记录任务执行结果
    if("__IMPORTING_FOLDER__" %in% ls(envir = TASK.ENV)) {
      tf <-  get_importing_folder()
    } else {
      tf <- "-"
    }
    state_write("__TASK_RUN__",
      tibble(
        "batchNum" = batchNum,
        "taskTopic" = taskTopic,
        "taskFolder" = taskFolder,
        "taskName" = name,
        "taskScript" = path,
        "importFolder" = tf,
        "usedTime" = used,
        "info" = msg
      )
    )
  })
}

#' @title 查看执行计划
#' @description 按执行顺序罗列需要执行的脚本。
#' @param taskTopic 脚本文件夹主题
#' @param taskFolder 执行脚本文件的目录
#' @param glob 要执行的源文件默认以.R结尾
#' @family task functions
#' @export
task_files <- function(taskTopic = "TASK/BUILD", taskFolder = "", glob = "*.R") {
  fs::dir_ls(get_path(taskTopic, taskFolder), recurse = T, glob = glob, type = "file") |>
    purrr::map_df(function(item) {
      name <- fs::path_file(item)
      list("name" = name, "path" = item)
    }) |>
    arrange(path)
}

#' @title 列举所有任务
#' @description 使用根目录、子目录管理脚本，目录名应具有管理约定的作用。
#' @details 
#' 任务执行脚本是数据处理工作流的核心管控单元。
#' 
#' 任务脚本的目录位置应与业务意义相对照。
#' 例如，导入和构建应分开，需要独立管理的导入单元应各自分开。
#' 
#' 任务执行时，可通过\code{taskScript}参数指定根目录或子目录，配合\code{task_run}函数批量执行。
#' 
#' @param taskTopic 脚本文件夹主题
#' @param taskFolder 执行脚本文件的目录
#' @param glob 要执行的源文件默认以.R结尾
#' @export
task_dir <- function(taskTopic = "TASK/BUILD", taskFolder = "", glob = "*.R") {
  task_read(taskTopic, taskFolder, glob) |>
    mutate(dir = fs::path_dir(path)) |>
    count(dir) |>
    mutate(taskName = stringr::str_remove(dir, paste0(get_path(taskScript), "/"))) |>
    select(taskName, dir, n)
}

#' @title 查看当前导入文件夹
#' @export
get_importing_folder <- function() {
  get_topic("__IMPORTING_FOLDER__")
}
