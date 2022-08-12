## 保存任务管理目录
TASK.ENV <- new.env(hash = TRUE)

#' @title 获取配置
#' @param topic 任务主题
#' @family TaskFolder functions
#' @export
get_topic <- function(topic) {
  get(topic, envir = TASK.ENV)
}

#' @title 获得所有主题
#' @family TaskFolder functions
#' @export
get_topics <- function() {
  ls(envir = TASK.ENV)
}

#' @title 根据配置构造数据路径
#' @param topic 任务主题
#' @family TaskFolder functions
#' @export
get_path <- function(topic, ...) {
  p <- list(...)
  fs::path_join(c(get_topic(topic), unlist(p)))
}

#' @title 设置主题目录
#' @param topic 任务主题
#' @param path 文件夹位置
#' @family TaskFolder functions
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
#' @export
taskfolder_todo <- function(taskFolder = "IMPORT", taskScript = "TASK/IMPORT") {
  tasks <- find_tasks(taskFolder)
  s <- read_state("__TASK_FOLDER__")
  if(!rlang::is_empty(s)) {
    d <- s |> select(taskFolder) |> collect()
    tasks[tasks %nin% d$taskFolder] |> batch_tasks(taskFolder, taskScript)
  } else {
    tasks |> batch_tasks(taskFolder, taskScript)
  }
}

#' @title 手工指定要处理的任务文件夹
#' @export
taskfolder_redo <- function(todo = c(), taskTopic = "IMPORT", taskScript = "TASK/IMPORT") {
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
  taskfolders |> purrr::walk(function(item) {
    set_topic("__DOING_TASK_FOLDER__", item)
    message("扫描任务文件夹：", item)
    run_task_scripts(taskScript, batch = T)
    set_topic("__DOING_TASK_FOLDER__", NULL)
    write_state("__TASK_FOLDER__", tibble(
      "taskTopic" = taskTopic,
      "taskFolder" = item,
      "status" = "DONE",
      "taskScript" = taskScript
    ))
  })
}

#' @title 执行目标路径下的任务脚本
#' @description 应当按照脚本顺序执行
#' @family TaskFolder functions
#' @export
run_task_scripts <- function(taskScript = "TASK/IMPORT", batch = F) {
  get_task_scripts(taskScript) |> purrr::pwalk(function(name, path) {
    message("执行任务：", name)
    beginTime <- lubridate::now()
    # 执行脚本
    source(path)
    used <- lubridate::now() - beginTime
    msg <- paste0("任务耗时：", as.character.Date(used))
    message(msg)
    # 记录任务执行结果
    if(batch) {
      tf <-  get_topic("__DOING_TASK_FOLDER__")
    } else {
      tf <- "-"
    }
    write_state("__TASK_RUN__",
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
#' @family TaskFolder functions
#' @export
get_task_scripts <- function(taskScript, glob = "*.R") {
  fs::dir_ls(get_path(taskScript), recurse = T, glob = glob, type = "file") |>
    purrr::map_df(function(item) {
      name <- fs::path_file(item)
      list("name" = name, "path" = item)
    })
}

#' @title 定义从任务文件夹导入数据集的函数
#' @description 在定义增量导入任务时需要定义按数据集导入的函数
#' @details 
#' 所定义的导入函数，需要读取数据，就要组装数据所在位置，规则如下：
#' {IMPORT}/{__DOING_TASK_FOLDER__}/{dsName}
#' 其中，__DOING_TASK_FOLDER__就是当前正在处理的任务文件夹名称，
#' 而dsName就是任务文件夹下的数据集文件夹，数据应当放在dsName文件夹内
#' 
#' 举一个例子，如果IMPORT目录位置是："~/glimmer/IMPORT"
#' 要导入的数据位置是："~/glimmer/IMPORT/task1/mycsv/1.csv"
#' 其中，task1就是导入文件夹，mycsv就是数据集。
#' 
#' 考虑增量导入的情况，在新的数据到位后，
#' 可能多了一个这样的文件："~/glimmer/IMPORT/task2/mycsv/1.csv"
#' 此时要做的，就是针对task2进行导入。
#' 而glimmer包提供的taskfolder_todo函数就是自动识别出新增的task2，并执行导入脚本。
#' 
#' @param dsName 要导入的数据集名称
#' @param fun 导入函数定义，要求是一个匿名函数
#' @param topic 导入文件夹所在的主题
#' @family TaskFolder functions
#' @export
ds_import <- function(dsName, fun, topic = "IMPORT") {
  path <- get_path(topic, get_topic("__DOING_TASK_FOLDER__"), dsName)
  if(path |> fs::dir_exists()) {
    fun(path)
  }
}
