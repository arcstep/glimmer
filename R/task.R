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
#' 在任务文件夹迭代处理时，每次处理完成就执行：
#' set_topic("__DOING_TASK_FOLDER__", item)
#' 
#' 具体的任务脚本中，例如导入数据，按如下方法判断是否有数据集可导入：
#' get_path("IMPORT", get_topic("__DOING_TASK_FOLDER__"), "{dataset_name}") |> fs::dir_exists()
#' @family task functions
#' @export
import_todo <- function(taskFolder = "IMPORT", taskScript = "TASK/IMPORT") {
  tasks <- find_tasks(taskFolder)
  s <- state_read("__TASK_FOLDER__")
  if(!rlang::is_empty(s)) {
    d <- s |> select(taskFolder) |> collect()
    tasks[tasks %nin% d$taskFolder] |> batch_tasks(taskFolder, taskScript)
  } else {
    tasks |> batch_tasks(taskFolder, taskScript)
  }
}

#' @title 手工指定要处理的任务文件夹
#' @family task functions
#' @export
import_redo <- function(todo = c(), taskTopic = "IMPORT", taskScript = "TASK/IMPORT") {
  taskfolders <- find_tasks(taskTopic)
  taskfolders[taskfolders %in% todo] |>  batch_tasks(taskTopic, taskScript)
}

#
find_tasks <- function(taskTopic) {
  path = get_path(taskTopic)
  fs::dir_ls(path, type = "directory", recurse = FALSE) |>
    sort() |>
    fs::path_file()
}

# 枚举任务文件夹
batch_tasks <- function(taskfolders, taskTopic, taskScript) {
  message(length(taskfolders), " task folders todo.")
  taskfolders |> purrr::walk(function(item) {
    set_topic("__DOING_TASK_FOLDER__", item)
    message("SCAN TASK FOLDER：", item)
    task_run(taskScript, batch = T)
    set_topic("__DOING_TASK_FOLDER__", NULL)
    state_write("__TASK_FOLDER__", tibble(
      "taskTopic" = taskTopic,
      "taskFolder" = item,
      "status" = "DONE",
      "taskScript" = taskScript
    ))
  })
}

#' @title 执行目标路径下的任务脚本
#' @description 应当按照脚本顺序执行
#' @family task functions
#' @export
task_run <- function(taskScript = "TASK/BUILD", batch = F) {
  task_read(taskScript) |> purrr::pwalk(function(name, path) {
    message("RUN TASK SCRIPT：", name)
    beginTime <- lubridate::now(tz = "Asia/Shanghai")
    # 执行脚本
    source(path)
    used <- lubridate::now(tz = "Asia/Shanghai") - beginTime
    msg <- paste0("TASK USED：", as.character.Date(used))
    message(msg)
    # 记录任务执行结果
    if(batch) {
      tf <-  get_topic("__DOING_TASK_FOLDER__")
    } else {
      tf <- "-"
    }
    state_write("__TASK_RUN__",
      tibble(
        "name" = name,
        "script" = path,
        "taskFolder" = tf,
        "usedTime" = used,
        "info" = msg
      )
    )
  })
}

#' @title 查看执行计划
#' @description 按执行顺序罗列需要执行的脚本
#' @param taskScript 脚本文件夹主题
#' @param glob 要执行的源文件默认以.R结尾
#' @family task functions
#' @export
task_read <- function(taskScript, glob = "*.R") {
  fs::dir_ls(get_path(taskScript), recurse = T, glob = glob, type = "file") |>
    purrr::map_df(function(item) {
      name <- fs::path_file(item)
      list("name" = name, "path" = item)
    })
}
