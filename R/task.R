## 获取任务快照的临时ID
getTaskSnapPath <- function(snapId, taskTopic = "TASK_DEFINE", snapTopic = "SNAP") {
  if(is.null(snapId)) {
    stop("snapId Not Exist !!")
  }
  if(length(snapId) != 1) {
    stop("snapId length MUST be 1 >> ", snapId |> paste(collapse = ","))
  }
  get_path(snapTopic, taskTopic, snapId, "main.rds")
}

##
getTaskPath <- function(taskId, taskTopic = "TASK_DEFINE") {
  if(is.null(taskId)) {
    stop("taskId Not Exist !!")
  }
  if(length(taskId) != 1) {
    stop("taskId length MUST be 1 >> ", taskId |> paste(collapse = ","))
  }
  get_path(taskTopic, paste0(taskId, ".rds"))
}

#' @title 创建任务执行框架
#' @param taskTopic 保存任务定义的存储主题文件夹
#' @param taskType 任务类型
#' @param taskId 任务唯一标识，每个\code{taskId}会保存为一个独立文件
#' @param online 如果任务下线，在推荐可执行任务时将忽略
#' @param desc 任务描述
#' @family task-define function
#' @export
task_create <- function(taskId, online = FALSE, force = FALSE,
                        taskType = "__UNKNOWN__", desc = "-",
                        taskTopic = "TASK_DEFINE", scriptsTopic = "TASK_SCRIPTS", snapTopic = "SNAP",
                        queueDataset = "__TASK_QUEUE__",importDataset = "__IMPORT_FILES__",
                        cacheTopic = "CACHE", importTopic = "IMPORT", extention = list()) {
  meta <- list(
    "taskId" = taskId,
    "online" = online,
    "taskType" = taskType,
    "desc" = desc,
    "extention" = extention,
    "taskTopic" = taskTopic,
    "scriptsTopic" = scriptsTopic,
    "snapTopic" = snapTopic,
    "queueDataset" = queueDataset,
    "importDataset" =importDataset,
    "cacheTopic" = cacheTopic,
    "importTopic" = importTopic,
    "createdAt" = as_datetime(lubridate::now(), tz = "Asia/Shanghai") |> as.character()
  )
  path <- getTaskPath(taskId, taskTopic)
  if(force || !fs::file_exists(path)) {
    fs::path_dir(path) |> fs::dir_create()
    saveRDS(meta, path)
    return(taskId)
  } else {
    stop("Task Already Exist: ", taskId)
  }
}

#' @title 更新任务配置
#' @param taskTopic 保存任务定义的存储主题文件夹
#' @param taskType 任务类型
#' @param taskId 任务唯一标识，每个\code{taskId}会保存为一个独立文件
#' @param online 如果任务下线，在推荐可执行任务时将忽略
#' @param desc 任务描述
#' @family task-define function
#' @export
task_update <- function(taskId, ..., force = FALSE, taskTopic = "TASK_DEFINE") {
  path <- getTaskPath(taskId, taskTopic)
  if(fs::file_exists(path)) {
    meta <- readRDS(path)
    metaInfo <- list(...)
    names(metaInfo) |> purrr::walk(function(i) {
      if(force || i %in% names(meta)) {
        meta[[i]] <<- metaInfo[[i]]
      } else {
        warning("No <", i, "> in Task Meta: ", taskId)
      }
    })
    saveRDS(meta, path)
    return(taskId)
  } else {
    stop("Task Not Exist for Update: ", taskId)
  }
}

#' @title 任务上线
#' @family task-define function
#' @export
task_online <- purrr::partial(task_update, online = TRUE)

#' @title 任务下线
#' @family task-define function
#' @export
task_offline <- purrr::partial(task_update, online = FALSE)


#' @title 取消编辑模式
#' @family task-define function
#' @export
task_cancel_snap <- purrr::partial(task_update, snapId = NULL)

#' @title 进入编辑模式
#' @description 
#' 针对任务中的脚本可使用编辑模式，
#' 其余任务设置则直接更新任务定义。
#' @export
task_edit_snap <- function(taskId, taskTopic = "TASK_DEFINE", snapTopic = "SNAP") {
  path <- getTaskPath(taskId, taskTopic)
  if(fs::file_exists(path)) {
    meta <- readRDS(path)
    if(is.null(meta$snapId)) {
      ## 修改为编辑模式，并克隆任务到快照
      meta$snapId <- gen_batchNum()
      saveRDS(meta, path)
      ##
      pathSnap <- getTaskSnapPath(meta$snapId, taskTopic, snapTopic)
      fs::path_dir(pathSnap) |> fs::dir_create()
      saveRDS(meta, pathSnap)
    } else {
      warning("Already in Edit-Snap Mode: ", taskId)
    }
    return(taskId)
  } else {
    stop("No Task Define try to edit-snap: ", taskId)
  }
}

#' @title 放弃修改，重新编辑
#' @export
task_discard <- function(taskId, taskTopic = "TASK_DEFINE", snapTopic = "SNAP") {
  path <- getTaskPath(taskId, taskTopic)
  if(fs::file_exists(path)) {
    meta <- readRDS(path)
    if(!is.null(meta$snapId)) {
      ## 放弃编辑的内容，重新克隆任务到快照
      saveRDS(meta, getTaskSnapPath(meta$snapId, taskTopic, snapTopic))
    } else {
      warning("Try to Discard But Not in Editing Mode: ", taskId)
    }
    return(taskId)
  } else {
    stop("No Task Define try to discar: ", taskId)
  }
}

#' @title 保存修改内容，并继续编辑
#' @export
task_save <- function(taskId, taskTopic = "TASK_DEFINE", snapTopic = "SNAP") {
  path <- getTaskPath(taskId, taskTopic)
  if(fs::file_exists(path)) {
    meta <- readRDS(path)
    if(!is.null(meta$snapId)) {
      ## 将快照内容复制到任务定义文件，并继续编辑
      newMeta <- readRDS(getTaskSnapPath(meta$snapId, taskTopic, snapTopic))
      saveRDS(newMeta, path)
    } else {
      warning("No Snap Need to Save: ", taskId)
    }
    return(taskId)
  } else {
    stop("No Task Define try to save: ", taskId)
  }
}

#' @title 克隆任务
#' @export
task_clone <- function(taskId, newTaskId, taskTopic = "TASK_DEFINE", snapTopic = "SNAP") {
  path <- getTaskPath(taskId, taskTopic)
  if(fs::file_exists(path)) {
    meta <- readRDS(path)
    meta$snapId <- NULL
    pathClone <- getTaskPath(newTaskId, taskTopic)
    fs::path_dir(pathClone) |> fs::dir_create()
    saveRDS(meta, pathClone)
    return(newTaskId)
  } else {
    stop("No Task Define: ", taskId)
  }
}

#' @title 提交修改内容，并结束编辑
#' @export
task_submit <- function(taskId, taskTopic = "TASK_DEFINE", snapTopic = "SNAP") {
  path <- getTaskPath(taskId, taskTopic)
  if(fs::file_exists(path)) {
    meta <- readRDS(path)
    if(!is.null(meta$snapId)) {
      ## 将快照内容复制到任务定义文件，并结束编辑模式
      newMeta <- readRDS(getTaskSnapPath(meta$snapId, taskTopic, snapTopic))
      newMeta$snapId <- NULL
      saveRDS(newMeta, path)
    } else {
      warning("No Snap Need to Submit: ", taskId)
    }
    return(taskId)
  } else {
    stop("No Task Define: ", taskId)
  }
}

#' @title 删除任务
#' @export
task_remove <- function(taskId, taskTopic = "TASK_DEFINE") {
  path <- getTaskPath(taskId, taskTopic)
  if(fs::file_exists(path)) {
    fs::file_delete(path)
    message("Task Removed: ", taskId)
    return(taskId)
  } else {
    warning("No Task Define: ", taskId)
  }
}

#' @title 读取任务
#' @param taskId 任务标识
#' @param taskTopic 保存任务定义的存储主题文件夹
#' @family task-define function
#' @export
task_read <- function(taskId, snap = FALSE, taskTopic = "TASK_DEFINE", snapTopic = "SNAP") {
  path <- getTaskPath(taskId, taskTopic)
  if(fs::file_exists(path)) {
    x <- readRDS(path)
    x$task_path = path
    if(snap) {
      if(!is.null(x$snapId)) {
        pathSnap <- getTaskSnapPath(x$snapId, taskTopic, snapTopic)
        return(readRDS(pathSnap))
      } else {
        warning("Snap Not Exist for task: ", taskId)
        return(x)
      }
    } else {
      return(x)
    }
  } else {
    stop("No Task Define before reading: ", taskId)
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

#' @title 运行任务
#' @description 
#' \code{task_run}可以仅执行到第\code{task_run}步结束，但不包括附加脚本（如队列）
#' @param taskTopic 保存任务定义的存储主题文件夹
#' @param taskId 任务标识
#' @param withQueue 是否在运行队列中显示状态
#' @param snap 当snap为TRUE时，执行编辑模式下的任务快照
#' @param stepToRun 执行到第N条脚本即结束（默认为较大的10000条）
#' @param runMode 运行模式（默认为进程内执行，改为r或r_bg为子进程执行）
#' @family task-define function
#' @export
task_run <- function(taskId,
                     withQueue = FALSE,
                     withEnv = FALSE,
                     snap = FALSE,
                     stepToRun = 1e4L,
                     taskTopic = "TASK_DEFINE",
                     runMode = "in-process", ...) {
  paramInfo <- list(...)
  ## 设置运行环境
  batchId <- gen_batchNum()
  taskMeta <- task_read(taskId, snap = snap)
  if(is.na(taskMeta$items) || is_empty(taskMeta$items)) {
    warning("Empty Task: ", taskId)
    return(NULL)
  }
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
      "params" = NA,
      "globalVars" = list(list("taskId" = taskId, "batchId" = batchId)),
      "inputAssign" = NA,
      "outputAssign" = NA
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
      "params" = NA,
      "globalVars" = list(list("taskId" = taskId, "batchId" = batchId,
                           "yamlParams" = paramInfo |> yaml::as.yaml())),
      "inputAssign" = NA,
      "outputAssign" = NA
    )
    items <- rbind(
      item_run,
      taskMeta$items |> head(stepToRun),
      item_done
    )
  } else {
    ## 不使用队列记录运行状态
    items <- taskMeta$items |> head(stepToRun)
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

## 预定义schema中有默认的映射规则，且在任务定义中未指定新的映射
## 允许映射多个输入参数
getFuncInputAssign <- function(script, inputAssign) {
  result <- inputAssign
  get_fun_schema(script, "params")$items |> purrr::walk(function(item) {
    if(is.null(inputAssign[[item]])) {
      schemaAsgin <- get_fun_schema(script, "params", item)$inputAssign
      if(!is.null(schemaAsgin)) {
        result[[item]] <<- schemaAsgin
      }
    }
  })
  result
}

## 仅一个输出参数
getFuncOutputAssign <- function(script, outputAssign) {
  oa <- get_fun_schema(script, "outputAssign")$value
  if(!is.null(oa) && is_empty(outputAssign)) {
    oa
  } else {
    outputAssign
  }
}

taskToRun <- function(taskItems, withEnv, ...) {
  ## 子函数内定义一个设置返回值的函数，供执行环境使用
  TaskRun.ENV <- new.env(hash = TRUE)
  ## 提取task_run运行时参数，可以设置执行环境的变量值
  taskParams <- list(...)
  names(taskParams) |> purrr::walk(function(i) {
    assign(i, taskParams[[i]], envir = TaskRun.ENV)
  })
  ## 将结果保存在curResult
  curResult <- NULL
  ## 逐项执行子任务
  taskItems |>
    tibble::rowid_to_column("rowId") |>
    purrr::pwalk(function(rowId, script, params, globalVars, type, inputAssign, outputAssign) {
      ## 设置执行环境变量值
      myParams <- params %na% list()
      names(globalVars) |> purrr::walk(function(i) {
        assign(i, globalVars[[i]], envir = TaskRun.ENV)
      })
      if(type == "func") {
        ## 支持函数管道
        if(is_empty(get_fun_schema(script))) {
          stop("No Schema Defined for Function: ", script)
        }
        inputAssign <- getFuncInputAssign(script, inputAssign %na% list())
        outputAssign <- getFuncOutputAssign(script, outputAssign %na% NULL)
        ## 使用环境内全局变量映射入参
        names(inputAssign |> unlist()) |> purrr::walk(function(i) {
          ## 如果映射目标不存在：函数参数列表中无此参数或执行环境中无此全局变量，就忽略
          if(i %in% formalArgs(script) && inputAssign[[i]] %in% ls(TaskRun.ENV)) {
            myParams[[i]] <<- get(inputAssign[[i]], envir = TaskRun.ENV)
          } else {
            warning("item ", rowId, ": ", type, "/", script, " >> Input Assign had been Ignored: ", i)
          }
        })
        ## 将输出值保存在"result"
        tryCatch({
          curResult <<- do.call(script, args = myParams, envir = TaskRun.ENV)
        }, error = function(e) {
          stop(e, "item ", rowId, ": ", type, "/", script)
        })
      } else {
        if(type == "string") {
          ## 用于灵活处理短逻辑
          curResult <<- parse(text = script) |> eval(envir = TaskRun.ENV)
        } else if(type == "var") {
          ## 用于设置环境参数
        } else if(type == "queue") {
          ## 用于队列日志，不参与业务逻辑
          eval(script, envir = TaskRun.ENV)
        } else if(type == "expr") {
          ## 用于灵活处理短逻辑
          curResult <<- eval(script, envir = TaskRun.ENV)
        } else if(type == "file") {
          ## 较复杂的手动处理逻辑，可写入脚本文件中
          pathScript <- get_path(get("@task", envir = TaskRun.ENV)$scriptsTopic, script)
          if(!fs::file_exists(pathScript)) {
            stop("No such script file: ", pathScript)
          }
          curResult <<- parse(file = pathScript) |> eval(envir = TaskRun.ENV)
        } else if(type == "dir") {
          ## 将脚本文件组织到一个文件夹，支持嵌套
          ## 按照文件路径名称顺序执行
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
      outputAssign |> purrr::walk(function(i) {
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

#
task_run0 <- function(taskItems, withEnv, runMode, ...) {
  if(runMode == "r") {
    callr::r(taskToRun, args = list(taskItems, withEnv, ...))
  } else if(runMode == "r_bg"){
    callr::r_bg(taskToRun, args = list(taskItems, withEnv, ...))
  } else {
    do.call("taskToRun", args = list(taskItems, withEnv, ...))
  }
}