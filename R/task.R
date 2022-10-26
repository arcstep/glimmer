#' @title 创建任务执行框架
#' @param taskTopic 保存任务定义的存储主题文件夹
#' @param taskType 任务类型
#' @param taskId 任务唯一标识，每个\code{taskId}会保存为一个独立文件
#' @param runLevel 同一批次任务中，运行时的优先级
#' @param online 如果任务下线，在推荐可执行任务时将忽略
#' @param desc 任务描述
#' @family task-define function
#' @export
task_create <- function(taskId, online = FALSE,
                        items = NULL,
                        taskType = "__UNKNOWN__", desc = "-",
                        taskTopic = "TASK_DEFINE", scriptsTopic = "TASK_SCRIPTS",
                        queueDataset = "__TASK_QUEUE__",importDataset = "__IMPORT_FILES__",
                        cacheTopic = "CACHE", importTopic = "IMPORT", extention = list()) {
  path <- get_path(taskTopic, paste0(taskId, ".rds"))
  fs::path_dir(path) |> fs::dir_create()
  settings <- list(
    "taskId" = taskId,
    "online" = online,
    "taskType" = taskType,
    "desc" = desc,
    "extention" = extention,
    "taskTopic" = taskTopic,
    "scriptsTopic" = scriptsTopic,
    "queueDataset" = queueDataset,
    "importDataset" =importDataset,
    "cacheTopic" = cacheTopic,
    "importTopic" = importTopic,
    "createdAt" = as_datetime(lubridate::now(), tz = "Asia/Shanghai") |> as.character(),
    "items" = items %empty% tibble(
      "type" = "empty",
      "script" = "ls()",
      "params" = list(NULL),
      "inputAsign" = list(NULL),
      "outputAsign" = list(NULL)
    )
  )
  settings |> saveRDS(path)
  taskId
}

#' @title 增加子任务
#' @param taskId 任务标识
#' @param script 子任务的执行路径
#' @param params 执行脚本的参数映射
#' @param inputAsign 针对function和gali类型，使用执行环境内变量映射入参
#' @param outputAsign 保存子任务输出
#' @param type 可以是string,expr,function,gali,file,dir,empty等
#' @param taskTopic 任务主题存储位置
#' @family task-define function
#' @export
task_item_add <- function(
    taskId,
    script = "ls()",
    params = list(NULL),
    inputAsign = list(NULL),
    outputAsign = list(NULL),
    touchFiles = TRUE,
    taskTopic = "TASK_DEFINE",
    type = "empty") {
  path <- get_path(taskTopic, paste0(taskId, ".rds"))
  if(fs::file_exists(path)) {
    ## 写入任务定义配置文件
    meta <- readRDS(path)
    item <- tibble(
      "type" = type,
      "script" = script,
      "params" = list(params),
      "inputAsign" = list(inputAsign),
      "outputAsign" = list(outputAsign)
      )
    if(rlang::is_empty(meta$items)) {
      meta$items <- item
    } else {
      meta$items <- rbind(as_tibble(meta$items), item)
    }
    meta |> saveRDS(path)
    ## 使用模板创建脚本
    if(touchFiles) {
      if(type == "file") task_script_file_create(script)
      if(type == "dir") task_script_dir_create(script)
    }
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
task_gali_add <- purrr::partial(task_item_add, type = "gali")

#' @title 为任务增加函数子任务
#' @family task-define function
#' @export
task_func_add <- purrr::partial(task_item_add, type = "func")

#' @title 为任务增加字符串脚本子任务
#' @family task-define function
#' @export
task_string_add <- purrr::partial(task_item_add, type = "string")

#' @title 为任务增加文件脚本子任务
#' @family task-define function
#' @export
task_file_add <- purrr::partial(task_item_add, type = "file")

#' @title 为任务增加目录脚本子任务
#' @family task-define function
#' @export
task_dir_add <- purrr::partial(task_item_add, type = "dir")

#' @title 为任务增加表达式脚本子任务
#' @family task-define function
#' @export
task_expr_add <- purrr::partial(task_item_add, type = "expr")

#' @title 为任务增加空任务
#' @family task-define function
#' @export
task_empty_add <- purrr::partial(task_item_add, type = "empty")

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
    x$task_path = path
    x
  } else {
    warning("No Task Define: ", taskId)
    list("task_path" = path)
  }
}

#' @title 任务上线或下线
#' @param taskId 任务标识
#' @param taskTopic 保存任务定义的存储主题文件夹
#' @family task-define function
#' @export
task_online <- function(taskId, online = TRUE, taskTopic = "TASK_DEFINE") {
  if(length(taskId) != 1) {
    stop("taskId length MUST be 1 >> ", taskId |> paste(collapse = ","))
  }
  path <- get_path(taskTopic, paste0(taskId, ".rds"))
  if(fs::file_exists(path)) {
    x <- readRDS(path)
    x$online = online
    x |> saveRDS(path)
    message("Task <", taskTopic, "/", taskId, "> online: ", online, " !!")
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
          x$items <- list(x$items)
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
#' @param taskTopic 保存任务定义的存储主题文件夹
#' @param taskId 任务标识
#' @param withQueue 是否在运行队列中显示状态
#' @param runMode 运行模式（默认为进程内执行，改为r或r_bg为子进程执行）
#' @family task-define function
#' @export
task_run <- function(taskId,
                     withQueue = FALSE,
                     withEnv = FALSE,
                     taskTopic = "TASK_DEFINE",
                     runMode = "in-process", ...) {
  paramInfo <- list(...)
  ## 设置运行环境
  batchId <- gen_batchNum()
  taskMeta <- task_read(taskId)
  ## 支持队列日志
  if(withQueue) {
    item_run <- tibble(
      "type" = "queue",
      "script" = expression({
        ## expression in task-run
        item <- task_queue_item(
          id = batchId,
          taskId = taskId,
          taskTopic = `@task`$taskTopic)
        item |> mutate(`@from` = "task_run()") |> ds_append(`@task`$queueDataset, `@task`$cacheTopic)
        ##
      }),
      "params" = list(list("taskId" = taskId, "batchId" = batchId)),
      "inputAsign" = list(),
      "outputAsign" = list()
    )
    item_done <- tibble(
      "type" = "queue",
      "script" = expression({
        ## expression in task-run
        item <- task_queue_search(dsName = `@task`$queueDataset, cacheTopic = `@task`$cacheTopic) |>
          filter(id == batchId) |>
          mutate(todo = FALSE, doneAt = now(tzone = "Asia/Shanghai"))
        item |> mutate(`@from` = "task_run()") |> ds_write(`@task`$queueDataset, `@task`$cacheTopic)
        ##
      }),
      "params" = list(list("taskId" = taskId, "batchId" = batchId,
                           "yamlParams" = paramInfo |> yaml::as.yaml())),
      "inputAsign" = list(),
      "outputAsign" = list()
    )
    items <- rbind(
      item_run,
      task_read(taskId, taskMeta$taskTopic)$items,
      item_done
    )
  } else {
    ## 不使用队列记录运行状态
    items <- task_read(taskId, taskMeta$taskTopic)$items
  }

  tryCatch({
    task_run0(items, runMode, withEnv = withEnv, `@task` = taskMeta, ...)
  }, error = function(e) {
    stop(
      e,
      "task_run Failed: ",
      "<", taskId, "> ",
      paramInfo |> unlist() |> paste(collapse = ","))
  })
}

#
task_run0 <- function(taskItems, withEnv, runMode, ...) {
  taskToRun <- function(..., taskItems) {
    ## 子函数内定义一个设置返回值的函数，供内部使用
    TaskRun.ENV <- new.env(hash = TRUE)
    # TaskRun.ENV <- globalenv()
    ## 提取task_run运行时参数，可以设置执行环境的变量值
    taskParams <- list(...)
    names(taskParams) |> purrr::walk(function(i) {
      assign(i, taskParams[[i]], envir = TaskRun.ENV)
    })
    ## 将结果保存在curResult
    curResult <- NULL
    ## 逐项执行子任务
    taskItems |> tibble::rowid_to_column("rowId") |>
      purrr::pwalk(function(rowId, script, params, type, inputAsign, outputAsign) {
      if(type %in% c("gali", "func")) {
        ## 提取函数定义参数，无法匹配的参数
        myparam <- formalArgs(script)
        funcParam <- params[myparam[myparam %in% names(params)]] %empty% list()
        ## 设置执行环境变量值
        envParam <- params[names(params) %nin% myparam] %empty% list()
        names(envParam) |> purrr::walk(function(i) {
          assign(i, params[[i]], envir = TaskRun.ENV)
        })
        ## 按照预定义任务函数连接管道：自动赋值输入、输出
        if(!rlang::is_empty(get_fun_schema(script))) {
          ia <- get_fun_schema(script, "inputAsign")$items
          oa <- get_fun_schema(script, "outputAsign")$items
          ## 预定义schema中有默认的映射规则，且在任务定义中未指定新的映射
          ## 允许映射多个输入参数
          if(!is.null(ia) && ia %nin% names(inputAsign)) {
            ia |> purrr::walk(function(item) {
              inputAsign[[item]] <<- get_fun_schema(script, "inputAsign", item)$items
            })
          }
          ## 仅一个输出参数
          if(!is.null(oa) && is.null(outputAsign |> unlist())) {
            outputAsign <- oa
          }
        }
        ## 使用环境内变量覆盖入参
        names(inputAsign |> unlist()) |> purrr::walk(function(i) {
          ## 如果映射目标不存在，就忽略
          if(i %in% myparam && inputAsign[[i]] %in% ls(TaskRun.ENV)) {
            funcParam[[i]] <<- get(inputAsign[[i]], envir = TaskRun.ENV)
          } else {
            warning("item ", rowId, ": ", type, "/", script, " >> Input Asign had been Ignored: ", i)
          }
        })
        ## 将输出值保存在"result"
        tryCatch({
          curResult <<- do.call(script, args = funcParam, envir = TaskRun.ENV)
        }, error = function(e) {
          stop(e, "item ", rowId, ": ", type, "/", script)
        })
      } else {
        ## 提取子任务定义参数
        names(params) |> purrr::walk(function(i) {
          assign(i, params[[i]], envir = TaskRun.ENV)
        })
        ##
        if(type == "string") {
          curResult <<- parse(text = script) |> eval(envir = TaskRun.ENV)
        } else if(type == "empty") {
          ## 一般用于设置环境参数
        } else if(type == "queue") {
          eval(script, envir = TaskRun.ENV)
        } else if(type == "expr") {
          curResult <<- eval(script, envir = TaskRun.ENV)
        } else if(type == "file") {
          pathScript <- get_path(get("@task", envir = TaskRun.ENV)$scriptsTopic, script)
          if(!fs::file_exists(pathScript)) {
            stop("No such script file: ", pathScript)
          }
          curResult <<- parse(file = pathScript) |> eval(envir = TaskRun.ENV)
        } else if(type == "dir") {
          pathScripts <- get_path(get("@task", envir = TaskRun.ENV)$scriptsTopic, script)
          if(!fs::dir_exists(pathScripts)) {
            stop("No such script dir: ", pathScripts)
          }
          allFiles <- fs::dir_ls(pathScripts, type = "file", recurse = T, glob = "*.R")
          if(length(allFiles) == 0) {
            stop("None R file existing in scripts dir: ", pathScripts)
          }
          allFiles |> sort() |> purrr::walk(function(p) {
            curResult <<- parse(file = p) |> eval(envir = TaskRun.ENV)
          })
        } else {
          warning("UNKNOWN ScriptType: ", type)
        }        
      }
      ## 覆盖或创建输出目标
      outputAsign |> purrr::walk(function(i) {
        if(is.character(i) && length(i) == 1) {
          assign(i, curResult, envir = TaskRun.ENV)
        }
      })
    })
    if(withEnv) {
      list("result" = curResult, "env" = as.list(TaskRun.ENV))
    } else {
      curResult
    }
  }
  
  if(runMode == "r") {
    callr::r(taskToRun, args = list(..., "taskItems" = taskItems))
  } else if(runMode == "r_bg"){
    callr::r_bg(taskToRun, args = list(..., "taskItems" = taskItems))
  } else {
    do.call("taskToRun", args = list(..., "taskItems" = taskItems))
  }
}