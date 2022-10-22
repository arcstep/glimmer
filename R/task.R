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
                        taskType = "__UNKNOWN__", desc = "-",
                        taskTopic = "TASK_DEFINE", scriptsTopic = "TASK_SCRIPTS",
                        queueName = "__TASK_QUEUE__", cacheTopic = "CACHE",
                        extention = list()) {
  path <- get_path(taskTopic, paste0(taskId, ".rds"))
  fs::path_dir(path) |> fs::dir_create()
  settings <- list(
    "taskId" = taskId,
    "runLevel" = runLevel,
    "online" = online,
    "taskType" = taskType,
    "desc" = desc,
    "extention" = extention,
    "taskTopic" = taskTopic,
    "scriptsTopic" = scriptsTopic,
    "queueName" = queueName,
    "cacheTopic" = cacheTopic,
    "createdAt" = as_datetime(lubridate::now(), tz = "Asia/Shanghai") |> as.character()
  )
  settings |> saveRDS(path)
  taskId
}

#' @title 增加子任务
#' @param taskId 任务标识
#' @param taskScript 子任务的执行路径
#' @param params 子任务的参数设置
#' @param scriptType 可以是string,file,dir, 或其他系统内置函数
#' @family task-define function
#' @export
task_item_add <- function(
    taskId,
    taskScript,
    params = list(NULL),
    scriptType = "string") {
  path <- get_path(taskTopic, paste0(taskId, ".rds"))
  if(fs::file_exists(path)) {
    ## 写入任务定义配置文件
    meta <- readRDS(path)
    item <- tibble(
      "taskScript" = taskScript,
      "params" = list(params %empty% NULL),
      "scriptType" = scriptType)
    if(rlang::is_empty(meta$items)) {
      meta$items <- item
    } else {
      meta$items <- rbind(as_tibble(meta$items), item)
    }
    meta |> saveRDS(path)
    ## 使用模板创建脚本
    if(scriptType == "file") task_script_file_create(taskScript)
    if(scriptType == "dir") task_script_dir_create(taskScript)
    ##
    ## 支持管道定义
    taskId
  } else {
    stop("Can't Add Task Before Task Define: ", taskId)
  }
}

#' @title 为任务增加gali函数子任务
#' @family task-define function
#' @export
task_gali_add <- purrr::partial(task_item_add, scriptType = "gali")


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
  if(length(taskId) != 1) {
    stop("taskId length MUST be 1 >> ", taskId |> paste(collapse = ","))
  }
  path <- get_path(taskTopic, paste0(taskId, ".rds"))
  if(fs::file_exists(path)) {
    x <- readRDS(path)
    x$items <- as_tibble(x$items)
    x$task_path = path
    x
  } else {
    warning("No Task Define: ", taskId)
    list("task_path" = path)
  }
}

#' @title 列举所有任务定义
#' @param topic 主题域
#' @family task-define function
#' @export
task_search <- function(taskMatch = ".*", typeMatch = ".*", taskTopic = "TASK_DEFINE") {
  root_path <- get_path(taskTopic)
  if(fs::dir_exists(root_path)) {
    tasks <- fs::dir_ls(root_path, type = "file", all = T, glob = "*.rds", recurse = T)
    if(length(tasks) > 0) {
      tasks |>
        purrr::map_df(function(path) {
          x <- readRDS(path)
          x$itemsCount <- length(x$items[1])
          x$items <- list(as_tibble(x$items))
          x$extention <- list(x$extention)
          x$online <- x$online %empty% TRUE
          x
        }) |>
        filter(stringr::str_detect(taskId, taskMatch)) |>
        filter(stringr::str_detect(taskType, typeMatch)) |>
        select(taskId, online, taskType, itemsCount, extention, everything())
    } else {
      tibble()
    }
  } else {
    tibble()
  }
}

#' @title 查询任务ID
#' @family task-define function
#' @export
task_id <- function(taskMatch = ".*", taskTopic = "TASK_DEFINE") {
  root_path <- get_path(taskTopic)
  if(fs::dir_exists(root_path)) {
    tasks <- fs::dir_ls(root_path, type = "file", all = T, regexp = paste0(taskMatch, ".*\\.rds$"), recurse = T)
    if(length(tasks) > 0) {
      tasks |> stringr::str_remove(root_path) |>
        stringr::str_sub(2, -5)
    } else {
      character(0)
    }
  } else {
    character(0)
  }
}

#' @title 运行任务
#' @param taskId 任务标识
#' @param withQueue 是否在运行队列中显示状态
#' @param queueName 指定队列数据集
#' @param taskTopic 保存任务定义的存储主题文件夹
#' @param scriptsTopic 脚本文件保存位置
#' @param cacheTopic 数据集保存位置
#' @param runMode 运行模式（默认为进程内执行，改为r或r_bg为子进程执行）
#' @family task-define function
#' @export
task_run <- function(taskId,
                     withQueue = FALSE,
                     taskTopic = "TASK_DEFINE",
                     runMode = "in-process", ...) {
  paramInfo <- list(...)
  ## 设置运行环境
  batchId <- gen_batchNum()
  taskMeta <- task_read(taskId)
  if(withQueue) {
    item_run <- tibble(
      "scriptType" = "queue",
      "taskScript" = expression({
        item <- task_queue_item(
          id = batchId,
          taskId = taskId,
          taskTopic = taskTopic)
        item |> mutate(`@from` = "task_run()") |> ds_append(queueName, taskMeta$cacheTopic)
      }),
      "params" = list(list("taskId" = taskId, "batchId" = batchId, "queueName" = taskMeta$queueName,
                           "yamlParams" = paramInfo |> yaml::as.yaml(),
                           "taskTopic" = taskTopic, "cacheTopic" = taskMeta$cacheTopic))
    )
    item_done <- tibble(
      "scriptType" = "queue",
      "taskScript" = expression({
        item <- task_queue_search(dsName = queueName, cacheTopic = taskMeta$cacheTopic) |>
          filter(id == batchId) |>
          mutate(todo = FALSE, doneAt = now(tzone = "Asia/Shanghai"))
        item |> mutate(`@from` = "task_run()") |> ds_write(queueName, taskMeta$cacheTopic)
      }),
      "params" = list(list("taskId" = taskId, "batchId" = batchId, "queueName" = taskMeta$queueName,
                           "yamlParams" = paramInfo |> yaml::as.yaml(),
                           "taskTopic" = taskTopic, "cacheTopic" = taskMeta$cacheTopic))
    )    
    items <- rbind(
      item_run,
      task_read(taskId, taskTopic)$items,
      item_done
    )
  } else {
    ## 不使用队列记录运行状态
    items <- task_read(taskId, taskTopic)$items
  }

  tryCatch({
    task_run0(items, runMode, scriptsTopic = scriptsTopic, `@task` = taskMeta, ...)
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
  paramInfo <- list(...)
  ## 提取任务信息
  items <- tibble(
    "taskScript" = taskFile,
    "params" = list(params %empty% NULL),
    "scriptType" = "file")
  tryCatch({
    task_run0(items, runMode, scriptsTopic = scriptsTopic, ...)
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
  paramInfo <- list(...)
  ## 提取任务信息
  items <- tibble(
    "taskScript" = taskDir,
    "params" = list(params %empty% NULL),
    "scriptType" = "dir")
  tryCatch({
    task_run0(items, runMode, scriptsTopic = scriptsTopic, ...)
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
  paramInfo <- list(...)
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
  paramInfo <- list(...)
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

#' @title 快速运行gali_函数
#' @param taskExpr 脚本表达式
#' @family task-define function
#' @export
task_run_gali <- function(galiItem, runMode = "in-process", ...) {
  paramInfo <- list(...)
  ## 提取任务信息
  items <- tibble(
    "taskScript" = galiItem$taskScript,
    "params" = list(galiItem$params),
    "scriptType" = galiItem$scriptType)
  tryCatch({
    task_run0(items, runMode, ...)
  }, error = function(e) {
    stop(
      e,
      "task_run_gali Failed: ",
      "<", galiItem$taskScript, "> ",
      paramInfo |> unlist() |> paste(collapse = ","))
  })
}

#' @title 按内容运行任务
#' @param task 任务
#' @param runMode 运行模式（默认为进程内执行，改为r或r_bg为子进程执行）
#' @family task-define function
#' @export
task_run0 <- function(taskItems, runMode = "in-process", ...) {
  taskToRun <- function(..., taskItems) {
    ## 子函数内定义一个设置返回值的函数，供内部使用
    # TaskRun.ENV <- new.env(hash = TRUE)
    TaskRun.ENV <- globalenv()
    taskParams <- list(...)
    names(taskParams) |> purrr::walk(function(i) {
      assign(i, taskParams[[i]], envir = TaskRun.ENV)
    })
    if("@result" %nin% ls(envir = TaskRun.ENV)) {
      assign("@result", list(), envir = TaskRun.ENV)
    }
    ## s_OUTPUT 可将任务执行结果保存在指定变量中
    if("s_OUTPUT" %in% ls(envir = TaskRun.ENV)) {
      myoutput <- get("s_OUTPUT")
    } else {
      myoutput <- "@result"
    }
    ## 逐项执行子任务
    taskItems |> purrr::pwalk(function(taskScript, params, scriptType) {
      if(scriptType == "string") {
        names(params) |> purrr::walk(function(i) {
          assign(i, params[[i]], envir = TaskRun.ENV)
        })
        assign(myoutput,
               parse(text = taskScript) |> eval(envir = TaskRun.ENV),
               envir = TaskRun.ENV)
      } else if(scriptType == "queue") {
        names(params) |> purrr::walk(function(i) {
          assign(i, params[[i]], envir = TaskRun.ENV)
        })
        eval(taskScript, envir = TaskRun.ENV)
      } else if(scriptType == "expr") {
        names(params) |> purrr::walk(function(i) {
          assign(i, params[[i]], envir = TaskRun.ENV)
        })
        assign(myoutput,
               eval(taskScript, envir = TaskRun.ENV),
               envir = TaskRun.ENV)
      } else if(scriptType == "file") {
        names(params) |> purrr::walk(function(i) {
          assign(i, params[[i]], envir = TaskRun.ENV)
        })
        pathScripts <- get_path(get("scriptsTopic", envir = TaskRun.ENV), taskScript)
        if(!fs::file_exists(pathScripts)) {
          stop("No such script file: ", pathScripts)
        }
        assign(myoutput,
               parse(file = pathScripts) |> eval(envir = TaskRun.ENV),
               envir = TaskRun.ENV)
      } else if(scriptType == "dir") {
        names(params) |> purrr::walk(function(i) {
          assign(i, params[[i]], envir = TaskRun.ENV)
        })
        pathScripts <- get_path(get("scriptsTopic", envir = TaskRun.ENV), taskScript)
        if(!fs::dir_exists(pathScripts)) {
          stop("No such script dir: ", pathScripts)
        }
        allFiles <- fs::dir_ls(pathScripts, type = "file", recurse = T, glob = "*.R")
        if(length(allFiles) == 0) {
          stop("None R file existing in scripts dir: ", pathScripts)
        }
        allFiles |> sort() |> purrr::walk(function(p) {
          assign(myoutput,
                 parse(file = p) |> eval(envir = TaskRun.ENV),
                 envir = TaskRun.ENV)
          })
      } else if(scriptType == "gali") {
        myparam <- formalArgs(taskScript)
        galiParam <- params[myparam[myparam %in% names(params)]] %empty% list()
        envParam <- params[names(params) %nin% myparam] %empty% list()
        names(envParam) |> purrr::walk(function(i) {
          assign(i, params[[i]], envir = TaskRun.ENV)
        })
        assign(myoutput,
               do.call(taskScript, args = galiParam, quote = TRUE, envir = TaskRun.ENV),
               envir = TaskRun.ENV)
      } else {
        warning("UNKNOWN ScriptType: ", scriptType)
      }
    })
    get("@result", envir = TaskRun.ENV)
  }
  
  if(runMode == "r") {
    callr::r(taskToRun, args = list(..., "taskItems" = taskItems))
  } else if(runMode == "r_bg"){
    callr::r_bg(taskToRun, args = list(..., "taskItems" = taskItems))
  } else {
    do.call("taskToRun", args = list(..., "taskItems" = taskItems))
  }
}