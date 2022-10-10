#' @title 创建任务执行框架
#' @param taskId 任务唯一标识，每个\code{taskId}会保存为一个独立文件
#' @param runLevel 同一批次任务中，运行时的优先级
#' @param online 任务是否启用
#' @param taskType 任务类型
#' @param desc 任务描述
#' @param taskTopic 保存任务定义的存储主题文件夹
#' @family task-define function
#' @export
task_create <- function(taskId, runLevel = 500L, online = TRUE,
                        taskType = "__UNKNOWN__", desc = "-", taskTopic = "TASK_DEFINE") {
  path <- get_path(taskTopic, paste0(taskId, ".yml"))
  fs::path_dir(path) |> fs::dir_create()
  list(
    "taskId" = taskId,
    "runLevel" = runLevel,
    "online" = online,
    "taskType" = taskType,
    "desc" = desc,
    "taskTopic" = taskTopic,
    "createdAt" = as_datetime(lubridate::now(), tz = "Asia/Shanghai") |> as.character()
  ) |>
    yaml::write_yaml(path)
  taskId
}

#' @title 增加子任务
#' @param taskId 任务标识
#' @param taskScript 子任务的执行路径
#' @param params 子任务的参数设置
#' @param scriptType 可以是string,file或folder
#' @param taskTopic 保存任务定义的存储主题文件夹
#' @param scriptsTopic 保存R脚本定义的存储主题文件夹
#' @family task-define function
#' @export
task_item_add <- function(
    taskId,
    taskScript,
    params = list(NULL),
    scriptType = "string",
    taskTopic = "TASK_DEFINE",
    scriptsTopic = "TASK_SCRIPTS") {
  path <- get_path(taskTopic, paste0(taskId, ".yml"))
  if(fs::file_exists(path)) {
    ## 写入任务定义配置文件
    meta <- yaml::read_yaml(path)
    item <- tibble(
      "scriptsTopic" = scriptsTopic,
      "taskScript" = taskScript,
      "params" = list(params %empty% NULL),
      "scriptType" = scriptType)
    if(rlang::is_empty(meta$items)) {
      meta$items <- item
    } else {
      meta$items <- rbind(as_tibble(meta$items), item)
    }
    meta |> yaml::write_yaml(path)
    ## 使用模板创建脚本
    if(scriptType == "file") task_script_file_create(taskScript)
    if(scriptType == "dir") task_script_dir_create(taskScript)
    ## 支持管道定义
    taskId
  } else {
    stop("Can't Add Task Before Task Define: ", taskId)
  }
}

#' @title 创建脚本文件
#' @family task-define function
#' @export
task_script_file_create <- function(scriptFile, scriptsTopic = "TASK_SCRIPTS") {
  path <- get_path(scriptsTopic, scriptFile)
  if(!fs::file_exists(fs::path_dir(path))) {
    fs::dir_create(fs::path_dir(path))
  }
  if(!fs::file_exists(path)) {
    fs::file_touch(path)
  }
}

#' @title 创建脚本目录
#' @family task-define function
#' @export
task_script_dir_create <- function(scriptDir, scriptsTopic = "TASK_SCRIPTS") {
  path <- get_path(scriptsTopic, scriptDir)
  if(!fs::dir_exists(path)) {
    fs::dir_create(path)
  }
  if(!fs::file_exists(fs::path_join(c(path, "task.R")))) {
    fs::file_touch(fs::path_join(c(path, "task.R")))
  }
}

#' @title 读取任务
#' @param taskId 任务标识
#' @param taskTopic 保存任务定义的存储主题文件夹
#' @family task-define function
#' @export
task_read <- function(taskId, taskTopic = "TASK_DEFINE") {
  path <- get_path(taskTopic, paste0(taskId, ".yml"))
  if(fs::file_exists(path)) {
    x <- yaml::read_yaml(path)
    x$items <- as_tibble(x$items)
    x$yaml_path = path
    x
  } else {
    warning("No Task Define: ", taskId)
    list("yaml_path" = path)
  }
}

#' @title 列举所有任务定义
#' @param topic 主题域
#' @family task-define function
#' @export
task_all <- function(taskTopic = "TASK_DEFINE") {
  root_path <- get_path(taskTopic)
  if(fs::dir_exists(root_path)) {
    fs::dir_ls(root_path, type = "file", all = T, glob = "*.yml", recurse = T) |>
      purrr::map_df(function(path) {
        x <- yaml::read_yaml(path)
        list(
          "taskTopic" = x$taskTopic,
          "taskId" = x$taskId,
          "items" = x$items |> as_tibble(),
          "runLevel" = x$runLevel,
          "online" = x$online %empty% TRUE,
          "taskType" = x$taskType,
          "desc" = x$desc,
          "createdAt" = x$createdAt
        )
      })
  } else {
    tibble()
  }
}

#' @title 运行任务
#' @param taskId 任务标识
#' @param params 参数赋值
#' @param taskTopic 保存任务定义的存储主题文件夹
#' @param runMode 运行模式（默认为进程内执行，改为r或r_bg为子进程执行）
#' @family task-define function
#' @export
task_run <- function(taskId, taskTopic = "TASK_DEFINE", runMode = "in-process", ...) {
  toRun <- function(..., task) {
    ## 子函数内定义一个设置返回值的函数，供内部使用
    TaskRun.ENV <- new.env(hash = TRUE)
    taskParams <- list(...)
    names(taskParams) |> purrr::walk(function(i) {
      assign(i, taskParams[[i]], envir = TaskRun.ENV)
    })
    
    assign("output", list(), envir = TaskRun.ENV)
    ## 逐项执行子任务
    task$items |> purrr::pwalk(function(scriptsTopic, taskScripts, params, scriptType) {
      names(params) |> purrr::walk(function(i) {
        assign(i, params[[i]], envir = TaskRun.ENV)
      })
      if(scriptType == "string") {
        assign("output",
               parse(text = taskScripts) |> eval(envir = TaskRun.ENV),
               envir = TaskRun.ENV)
      } else if(scriptType == "file") {
        pathScripts <- get_path(scriptsTopic, taskScripts)
        if(!fs::file_exists(pathScripts)) {
          stop("No such script file: ", pathScripts)
        }
        assign("output",
               parse(file = pathScripts) |> eval(envir = TaskRun.ENV),
               envir = TaskRun.ENV)
      } else if(scriptType == "dir") {
        pathScripts <- get_path(scriptsTopic, taskScripts)
        if(!fs::dir_exists(pathScripts)) {
          stop("No such script dir: ", pathScripts)
        }
        allFiles <- fs::dir_ls(pathScripts, type = "file", recurse = T, glob = "*.R")
        if(length(allFiles) == 0) {
          stop("None R file existing in scripts dir: ", pathScripts)
        }
        allFiles |> sort() |> purrr::walk(function(p) {
          assign("output",
                 parse(file = p) |> eval(envir = TaskRun.ENV),
                 envir = TaskRun.ENV)
          })
      } else {
        warning("UNKNOWN ScriptType: ", scriptType)
      }
    })
    get("output", envir = TaskRun.ENV)
  }
  taskread <- task_read(taskId, taskTopic)
  
  if(runMode == "r") {
    callr::r(toRun, args = list(..., "task" = taskread))
  } else if(runMode == "r_bg"){
    callr::r_bg(toRun, args = list(..., "task" = taskread))
  } else {
    do.call("toRun", args = list(..., "task" = taskread))
  }
}