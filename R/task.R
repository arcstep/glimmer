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

#' @title 所有导入文件夹
#' @description 将主题域下的所有一级子目录作为导入素材
#' @param importTopic 导入主题域，即导入素材的根目录
#' @details 
#' 未导入成功过的文件夹可作为增量导入素材，被自动发现。
#' @family task functions
#' @export
import_folders <- function(importTopic = "IMPORT", taskTopic = "TASK/IMPORT") {
  root <- get_path(importTopic)
  all <- fs::dir_ls(root, type = "directory", recurse = FALSE) |>
    fs::file_info() |>
    select(path, size, modification_time, user, group, device_id, blocks, block_size) |>
    mutate(importFolder = stringr::str_remove(path, paste0(root, "/"))) |>
    mutate(importTopic = importTopic) |>
    select(importTopic, importFolder, everything())
  ## 取出所有导入文件夹的状态，用做增量对比
  s <- state_read("__IMPORTED_FOLDER__") |> collect()
  if(!rlang::is_empty(s)) {
    oldTasks <- s |> group_by(importTopic, importFolder) |>
      summarise(lastImportedAt = max(lastModified), .groups = "drop")
    all |> left_join(oldTasks, by = c("importTopic", "importFolder"))
  } else {
    all |> mutate(lastImportedAt = NA)
  }
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
  batchNum <- gen_batchNum()
  all <- import_folders(importTopic = importTopic, taskTopic = taskTopic)
  if(rlang::is_empty(all)) {
    warnnig("No Data Folders To IMPORT!")
  } else {
    (all |> filter(is.na(lastImportedAt)))$importFolder |>
      batch_tasks(importTopic = importTopic,
                  taskTopic = taskTopic,
                  taskFolder = taskFolder,
                  batchNum = batchNum)
  }
}

#' @title 手工指定要处理的任务文件夹
#' @family task functions
#' @export
import_redo <- function(todo = c(), importTopic = "IMPORT", taskTopic = "TASK/IMPORT", taskFolder = "") {
  batchNum <- gen_batchNum()
  todo |> batch_tasks(importTopic = importTopic,
                      taskTopic = taskTopic,
                      taskFolder = taskFolder,
                      batchNum = batchNum)
}

# 枚举任务文件夹
batch_tasks <- function(importFolders, importTopic, taskTopic, taskFolder, batchNum) {
  message(length(importFolders), " task folders todo.")
  importFolders |> purrr::walk(function(item) {
    set_topic("__IMPORTING_FOLDER__", item)
    message("SCAN IMPORT FOLDER：", item)
    task_run(
      taskTopic = taskTopic,
      taskFolder = taskFolder,
      importTopic = importTopic,
      batchNum = batchNum)
    set_topic("__IMPORTING_FOLDER__", "-")
    state_write("__IMPORTED_FOLDER__", tibble(
      "batchNum" = batchNum,
      "importTopic" = importTopic,
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
#' @param importTopic 导入主题域
#' @param glob 默认加载所有R文件
#' @details 
#' glob参数可用于运行运行特定的R文件，
#' 例如 \code{task_run(taskFolder = "abc", glob = "**/1.R")}。
#' @family task functions
#' @export
task_run <- function(
    taskTopic = "TASK/BUILD",
    taskFolder = "",
    importTopic = "IMPORT",
    batchNum = gen_batchNum(),
    glob = "*.R") {
  task_files(taskTopic, taskFolder, glob = glob) |> purrr::pwalk(function(topic, folder, name, path) {
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
        "importTopic" = importTopic,
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
  folder_path <- get_path(taskTopic, taskFolder)
  if(fs::dir_exists(folder_path)) {
    files <- fs::dir_ls(folder_path, recurse = T, glob = glob, type = "file")
    if(length(files) > 0) {
      message(length(files), " script files to run.")
      files |>
        purrr::map_df(function(item) {
          name <- item |> stringr::str_remove(folder_path) |> stringr::str_remove("^/")
          list("topic" = taskTopic, "folder" = taskFolder, "name" = name, "path" = item)
        }) |>
        arrange(path)
    } else {
      message("NO script files to run.")
      tibble()
    }
  } else {
    message("NO script dir and files to run.")
    tibble()
  }
}

#' @title 列举所有任务
#' @description 使用根目录、子目录管理脚本，目录名应具有管理约定的作用。
#' @details 
#' 任务执行脚本是数据处理工作流的核心管控单元。
#' 
#' 任务脚本的目录位置应与业务意义相对照。
#' 例如，导入和构建应分开，需要独立管理的导入单元应各自分开。
#' 
#' 任务执行时，可通过\code{taskTopic}参数指定根目录或子目录，配合\code{task_run}函数批量执行。
#' 
#' @param taskTopic 脚本文件夹主题
#' @param taskFolder 执行脚本文件的目录
#' @param glob 要执行的源文件默认以.R结尾
#' @family task functions
#' @export
task_dir <- function(taskTopic = "TASK/BUILD", taskFolder = "", glob = "*.R") {
  if(fs::dir_exists(get_path(taskTopic, taskFolder))) {
    files <- task_files(taskTopic, taskFolder, glob)
    if(nrow(files > 0)) {
      files|>
        mutate("task" = fs::path_dir(name)) |>
        mutate("folder_path" = fs::path_dir(path)) |>
        group_by(topic, folder, task, folder_path) |>
        summarise(n = n(), .groups = "drop")
    } else {
      tibble()
    }
  } else {
    tibble()
  }
}

#' @title 查看当前导入文件夹
#' @family task functions
#' @export
get_importing_folder <- function() {
  get_topic("__IMPORTING_FOLDER__")
}
