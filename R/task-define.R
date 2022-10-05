#' @title 创建任务执行框架
#' @param taskId 任务唯一标识，每个\code{taskId}会保存为一个独立文件
#' @param items 子任务清单，包含子任务的执行脚本、参数设定等
#' @param taskType 任务类型
#' @param desc 任务描述
#' @param taskTopic 保存任务定义的存储主题文件夹
#' @param family task-define function
#' @export
task_create <- function(taskId, items = tibble(), taskType = "__UNKNOWN__", desc = "-", taskTopic = "TASK_DEFINE") {
  path <- get_path(taskTopic, paste0(taskId, ".yml"))
  fs::path_dir(path) |> fs::dir_create()
  list(
    "taskId" = taskId,
    "items" = items,
    "taskType" = taskType,
    "desc" = desc,
    "taskTopic" = taskTopic,
    "createdAt" = lubridate::as_datetime(lubridate::now(), tz = "Asia/Shanghai")
  ) |>
    yaml::write_yaml(path)
}

#' @title 增加子任务
#' @param taskId 任务标识
#' @param taskScript 子任务的执行路径
#' @param params 子任务的参数设置
#' @param scriptType 可以是string,file或folder
#' @param taskTopic 保存任务定义的存储主题文件夹
#' @param family task-define function
#' @export
task_add <- function(
    taskId,
    taskScript,
    params = list(NULL),
    scriptType = "string",
    taskTopic = "TASK_DEFINE") {
  path <- get_path(taskTopic, paste0(taskId, ".yml"))
  if(fs::file_exists(path)) {
    meta <- yaml::read_yaml(path)
    item <- tibble(
      "taskScript" = taskScript,
      "params" = list(params %empty% NULL),
      "scriptType" = scriptType)
    if(rlang::is_empty(meta$items)) {
      meta$items <- item
    } else {
      meta$items <- rbind(as_tibble(meta$items), item)
    }
    meta |> yaml::write_yaml(path)
  } else {
    stop("Can't Add Task Before Task Define: ", taskId)
  }
}

#' @title 读取任务
#' @param taskId 任务标识
#' @param taskTopic 保存任务定义的存储主题文件夹
#' @param family task-define function
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

#' @title 运行任务
#' @param taskId 任务标识
#' @param params 参数赋值
#' @param taskTopic 保存任务定义的存储主题文件夹
#' @param family task-define function
#' @export
task_run <- function(taskId, taskTopic = "TASK_DEFINE", runMode = "in-process", ...) {
  toRun <- function(..., task) {
    ## 子函数内定义一个设置返回值的函数，供内部使用
    TaskRun.ENV <- new.env(hash = TRUE)
    
    assign("result", list(), envir = TaskRun.ENV)
    ## 逐项执行子任务
    task$items |> purrr::pwalk(function(taskScripts, params, scriptType) {
      names(params) |> purrr::walk(function(i) {
        assign(i, params[[i]], envir = TaskRun.ENV)
      })
      if(scriptType == "string") {
        parse(text = taskScripts) |> eval(envir = TaskRun.ENV)
      } else if(scriptType == "file") {
        parse(file = taskScripts) |> eval(envir = TaskRun.ENV)
      } else if(scriptType == "folder") {
        fs::dir_ls(taskScripts, type = "file", recurse = T, glob = "*.R") |>
          sort() |>
          purrr::walk(function(p) parse(file = p) |> eval(envir = TaskRun.ENV))
      } else {
        warning("UNKNOWN ScriptType: ", scriptType)
      }
    })
    get("result", envir = TaskRun.ENV)
  }
  taskread <- task_read(taskId, taskTopic)
  
  if(runMode == "r") {
    callr::r(toRun, args = list(..., "task" = taskread))
  } else if(runMode == "r_bg"){
    callr::r_bg(toRun, args = list(..., "task" = taskread))
  } else {
    do.call("toRun", args = list(..., "task" = task_read(taskId, taskTopic)))
  }
}