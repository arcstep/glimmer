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
                        taskType = "__UNKNOWN__", desc = "-", taskTopic = "TASK_DEFINE", extention = list()) {
  path <- get_path(taskTopic, paste0(taskId, ".yml"))
  fs::path_dir(path) |> fs::dir_create()
  params <- list(...)
  yml <- list(
    "taskId" = taskId,
    "runLevel" = runLevel,
    "online" = online,
    "taskType" = taskType,
    "desc" = desc,
    "extention" = extention,
    "taskTopic" = taskTopic,
    "createdAt" = as_datetime(lubridate::now(), tz = "Asia/Shanghai") |> as.character()
  )
  names(params) |> purrr::walk(function(n) yml[[n]] <<- params[[n]])
  yml |> yaml::write_yaml(path)
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
task_search <- function(taskMatch = ".*", typeMatch = ".*", taskTopic = "TASK_DEFINE") {
  root_path <- get_path(taskTopic)
  if(fs::dir_exists(root_path)) {
    tasks <- fs::dir_ls(root_path, type = "file", all = T, glob = "*.yml", recurse = T)
    if(length(tasks) > 0) {
      tasks |>
        purrr::map_df(function(path) {
          x <- yaml::read_yaml(path)
          list(
            "taskTopic" = x$taskTopic,
            "taskId" = x$taskId,
            "itemsCount" = x$items |> as_tibble() |> nrow(),
            "runLevel" = x$runLevel,
            "online" = x$online %empty% TRUE,
            "taskType" = x$taskType,
            "desc" = x$desc,
            "createdAt" = x$createdAt
          )
        }) |>
        filter(stringr::str_detect(taskId, taskMatch)) |>
        filter(stringr::str_detect(taskType, typeMatch))
    } else {
      tibble()
    }
  } else {
    tibble()
  }
}

#' @title 运行任务
#' @param taskId 任务标识
#' @param taskTopic 保存任务定义的存储主题文件夹
#' @param runMode 运行模式（默认为进程内执行，改为r或r_bg为子进程执行）
#' @family task-define function
#' @export
task_run <- function(taskId,
                     taskTopic = "TASK_DEFINE",
                     scriptsTopic = "TASK_SCRIPTS",
                     cacheTopic = "CACHE",
                     queueName = "__TASK_QUEUE__",
                     runMode = "in-process", ...) {
  paramInfo <- list(...)
  ## 提取任务信息
  batchId <- gen_batchNum()
  item_run <- tibble(
    "scriptsTopic" = scriptsTopic,
    "taskScript" = expression({
      item <- task_queue_item(
        taskId = taskId,
        id = batchId,
        yamlParams = yamlParams,
        taskTopic = taskTopic,
        cacheTopic = cacheTopic)
      item |> ds_append(queueName, cacheTopic)
    }),
    "params" = list(list("taskId" = taskId,
                    "batchId" = batchId,
                    "queueName" = queueName,
                    "yamlParams" = paramInfo |> task_queue_param_to_yaml(),
                    "taskTopic" = taskTopic,
                    "cacheTopic" = cacheTopic)),
    "scriptType" = "queue"
  )
  item_done <- tibble(
    "scriptsTopic" = scriptsTopic,
    "taskScript" = expression({
      item <- task_queue_search(dsName = queueName, cacheTopic = cacheTopic) |>
        filter(id == batchId) |>
        mutate(doneAt = now(tzone = "Asia/Shanghai"))
      item |> ds_write(queueName, cacheTopic)
    }),
    "params" = list(list("taskId" = taskId, "batchId" = batchId, "queueName" = queueName,
                    "yamlParams" = paramInfo |> task_queue_param_to_yaml(),
                    "taskTopic" = taskTopic, "cacheTopic" = cacheTopic)),
    "scriptType" = "queue"
  )
  items <- rbind(
    item_run,
    task_read(taskId, taskTopic)$items,
    item_done
  )
  tryCatch({
    task_run0(items, runMode, ...)
  }, error = function(e) {
    stop(
      e,
      "task_run Failed: ",
      "<", taskId, "> ",
      paramInfo |> unlist() |> paste(collapse = ","))
  })
}


#' @title 快速运行脚本文件
#' @param taskFile 任务文件路径
#' @family task-define function
#' @export
task_run_file <- function(taskFile, params = list(NULL), scriptsTopic = "TASK_SCRIPTS", runMode = "in-process", ...) {
  ## 提取任务信息
  items <- tibble(
    "scriptsTopic" = scriptsTopic,
    "taskScript" = taskFile,
    "params" = list(params %empty% NULL),
    "scriptType" = "file")
  tryCatch({
    task_run0(items, runMode, ...)
  }, error = function(e) {
    stop(
      e,
      "task_run_file Failed: ",
      "<", taskFile, "> ",
      paramInfo |> unlist() |> paste(collapse = ","))
  })
}

#' @title 快速运行脚本文件夹
#' @param taskDir 任务文件夹路径
#' @family task-define function
#' @export
task_run_dir <- function(taskDir, params = list(NULL), scriptsTopic = "TASK_SCRIPTS", runMode = "in-process", ...) {
  ## 提取任务信息
  items <- tibble(
    "scriptsTopic" = scriptsTopic,
    "taskScript" = taskDir,
    "params" = list(params %empty% NULL),
    "scriptType" = "dir")
  tryCatch({
    task_run0(items, runMode, ...)
  }, error = function(e) {
    stop(
      e,
      "task_run_dir Failed: ",
      "<", taskDir, "> ",
      paramInfo |> unlist() |> paste(collapse = ","))
  })
}

#' @title 快速运行脚本字符串
#' @param taskString 脚本字符串
#' @family task-define function
#' @export
task_run_string <- function(taskString, params = list(NULL), runMode = "in-process", ...) {
  ## 提取任务信息
  items <- tibble(
    "taskScript" = taskString,
    "params" = list(params %empty% NULL),
    "scriptType" = "string")
  tryCatch({
    task_run0(items, runMode, ...)
  }, error = function(e) {
    stop(
      e,
      "task_run_string Failed: ",
      "<", taskString, "> ",
      paramInfo |> unlist() |> paste(collapse = ","))
  })
}

#' @title 快速运行表达式
#' @param taskExpr 脚本表达式
#' @family task-define function
#' @export
task_run_expr <- function(taskExpr, params = list(NULL), runMode = "in-process", ...) {
  ## 提取任务信息
  items <- tibble(
    "taskScript" = taskExpr,
    "params" = list(params %empty% NULL),
    "scriptType" = "expr")
  tryCatch({
    task_run0(items, runMode, ...)
  }, error = function(e) {
    stop(
      e,
      "task_run_expr Failed: ",
      "<", taskExpr, "> ",
      paramInfo |> unlist() |> paste(collapse = ","))
  })
}

#' @title 按内容运行任务
#' @param task 任务
#' @param runMode 运行模式（默认为进程内执行，改为r或r_bg为子进程执行）
#' @family task-define function
#' @export
task_run0 <- function(taskItems, runMode = "in-process", ...) {
  toRun <- function(..., taskItems) {
    ## 子函数内定义一个设置返回值的函数，供内部使用
    TaskRun.ENV <- new.env(hash = TRUE)
    taskParams <- list(...)
    names(taskParams) |> purrr::walk(function(i) {
      assign(i, taskParams[[i]], envir = TaskRun.ENV)
    })
    
    assign("output", list(), envir = TaskRun.ENV)
    ## 逐项执行子任务
    taskItems |> purrr::pwalk(function(scriptsTopic, taskScripts, params, scriptType) {
      names(params) |> purrr::walk(function(i) {
        assign(i, params[[i]], envir = TaskRun.ENV)
      })
      if(scriptType == "string") {
        assign("output",
               parse(text = taskScripts) |> eval(envir = TaskRun.ENV),
               envir = TaskRun.ENV)
      } else if(scriptType == "queue") {
        eval(taskScripts, envir = TaskRun.ENV)
      } else if(scriptType %in% c("expr", "filter")) {
        assign("output",
               eval(taskScripts, envir = TaskRun.ENV),
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
  
  if(runMode == "r") {
    callr::r(toRun, args = list(..., "taskItems" = taskItems))
  } else if(runMode == "r_bg"){
    callr::r_bg(toRun, args = list(..., "taskItems" = taskItems))
  } else {
    do.call("toRun", args = list(..., "taskItems" = taskItems))
  }
}