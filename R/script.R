
#' @title 增加子任务
#' @description 
#' 创建任务过程中变量赋值顺序是：globalVars, inputAssign, outputAssign
#' 
#' 如果添加的func类型，还应当注意：
#' \itemize{
#' \item 要在func脚本中使用globalVars，需要设置inputAssign映射
#' \item func脚本中的inputAssign设置可能有3种来源：
#'       （1）是\code{@}开头的全局变量；
#'       （2）是\code{globalVars}或\code{outputAssign}输出的全局变量；
#'       （3）是执行\code{task_run}时额外提供的参数。
#' } 
#' 
#' @param taskName 任务标识
#' @param script 执行脚本或函数名、文件名、目录名
#' @param params 执行函数参数映射
#' @param globalVars 设置全局变量
#' @param inputAssign 使用全局变量或任务参数映射脚本入参
#' @param outputAssign 将输出结果保存为全局变量
#' @param touchFiles 类型为file,dir时自动创建脚本文件
#' @param type 可以是string,expr,function,file,dir,var等
#' @param taskTopic 任务主题存储位置
#' @family task-script function
#' @export
script_item_add <- function(
    taskName,
    script = NA,
    params = NA,
    globalVars = NA,
    inputAssign = NA,
    outputAssign = NA,
    touchFiles = TRUE,
    taskTopic = "TASK_DEFINE",
    type = "var") {
  path <- getTaskPath(taskName, taskTopic)
  if(fs::file_exists(path)) {
    ## 任务定义配置文件
    meta <- readRDS(path)
    if(!is.null(meta$snapId)) {
      ## 在编辑模式中
      path <- getTaskSnapPath(meta$snapId, taskTopic, meta$snapTopic)
      meta <- readRDS(path)
    }
  } else {
    stop("Can't Add Task Scripts Before Task Define: ", taskName)
  }
  item <- tibble(
    "type" = type,
    "script" = script,
    "params" = params %not-na% list(params),
    "globalVars" = globalVars %not-na% list(globalVars),
    "inputAssign" = inputAssign %not-na% list(inputAssign),
    "outputAssign" = outputAssign %not-na% list(outputAssign)
  )
  if(is_empty(meta$items)) {
    meta$items <- item
  } else {
    meta$items <- rbind(meta$items, item)
  }
  meta |> saveRDS(path)
  ## 使用模板创建脚本
  if(touchFiles) {
    if(type == "file") task_script_file_create(script)
    if(type == "dir") task_script_dir_create(script)
  }
  ## 支持管道定义
  taskName
}

# 创建脚本文件
task_script_file_create <- function(scriptFile, scriptsTopic = "TASK_SCRIPTS") {
  path <- get_path(scriptsTopic, scriptFile)
  if(!fs::file_exists(fs::path_dir(path))) {
    fs::dir_create(fs::path_dir(path))
  }
  if(!fs::file_exists(path)) {
    fs::file_touch(path)
  }
}

# 创建脚本目录
task_script_dir_create <- function(scriptDir, scriptsTopic = "TASK_SCRIPTS") {
  path <- get_path(scriptsTopic, scriptDir)
  if(!fs::dir_exists(path)) {
    fs::dir_create(path)
  }
  if(!fs::file_exists(fs::path_join(c(path, "task.R")))) {
    fs::file_touch(fs::path_join(c(path, "task.R")))
  }
}


#' @title 更新任务脚本
#' @param taskName 任务标识
#' @param rowNum 要更新的脚本序号
#' @param script 执行脚本或函数名、文件名、目录名
#' @param params 执行函数参数映射
#' @param globalVars 设置全局变量
#' @param inputAssign 针对function和gali类型，使用执行环境内变量映射入参
#' @param outputAssign 保存子任务输出
#' @param touchFiles 类型为file,dir时自动创建脚本文件
#' @param type 可以是string,expr,function,gali,file,dir,global等
#' @param taskTopic 任务主题存储位置
#' @family task-script function
#' @export
script_item_update <- function(
    taskName,
    rowNum,
    script = NA,
    params = NA,
    globalVars = NA,
    inputAssign = NA,
    outputAssign = NA,
    touchFiles = TRUE,
    taskTopic = "TASK_DEFINE",
    type = "global") {
  path <- getTaskPath(taskName, taskTopic)
  if(fs::file_exists(path)) {
    meta <- readRDS(path)
    if(!is.null(meta$snapId)) {
      ## 在编辑模式中
      path <- getTaskSnapPath(meta$snapId, taskTopic, meta$snapTopic)
      meta <- readRDS(path)
    }
  } else {
    stop("Can't Update Task Scripts Before Task Define: ", taskName)
  }
  item <- tibble(
    "type" = type,
    "script" = script,
    "params" = params %not-na% list(params),
    "globalVars" = globalVars %not-na% list(globalVars),
    "inputAssign" = inputAssign %not-na% list(inputAssign),
    "outputAssign" = outputAssign %not-na% list(outputAssign)
  )
  if(rowNum >= 1 && rowNum <= nrow(meta$items)) {
    meta$items <- rbind(head(meta$item, rowNum - 1),
                        item,
                        tail(meta$item, nrow(meta$item) - rowNum))
  } else {
    stop("rowNum [", rowNum, "] is invalid for task: ", taskName)
  }
  meta |> saveRDS(path)
  ## 使用模板创建脚本
  if(touchFiles) {
    if(type == "file") script_script_file_create(script)
    if(type == "dir") script_script_dir_create(script)
  }
  ## 支持管道定义
  taskName
}

#' @title 删除任务脚本
#' @param taskName 任务标识
#' @param rowNum 要更新的脚本序号
#' @param taskTopic 任务主题存储位置
#' @family task-script function
#' @export
script_item_remove <- function(
    taskName,
    rowNum,
    taskTopic = "TASK_DEFINE") {
  path <- getTaskPath(taskName, taskTopic)
  if(fs::file_exists(path)) {
    meta <- readRDS(path)
    if(!is.null(meta$snapId)) {
      path <- getTaskSnapPath(meta$snapId, taskTopic, meta$snapTopic)
      meta <- readRDS(path)
    }
  } else {
    stop("Can't Remove Task Scripts Before Task Define: ", taskName)
  }
  if(rowNum >= 1 && rowNum <= nrow(meta$items)) {
    meta$items <- rbind(head(meta$item, rowNum - 1),
                        tail(meta$item, nrow(meta$item) - rowNum))
  } else {
    stop("rowNum [", rowNum, "] is invalid for task: ", taskName)
  }
  meta |> saveRDS(path)
  ## 支持管道定义
  taskName
}


#' @title 调整任务脚本顺序
#' @param taskName 任务标识
#' @param rowNumOld 脚本旧序号
#' @param rowNumNew 脚本新序号
#' @param taskTopic 任务主题存储位置
#' @family task-script function
#' @export
script_item_exchange <- function(
    taskName,
    rowNumA,
    rowNumB,
    taskTopic = "TASK_DEFINE") {
  path <- getTaskPath(taskName, taskTopic)
  if(fs::file_exists(path)) {
    meta <- readRDS(path)
    if(!is.null(meta$snapId)) {
      path <- getTaskSnapPath(meta$snapId, taskTopic, meta$snapTopic)
      meta <- readRDS(path)
    }
  } else {
    stop("Can't Remove Task Scripts Before Task Define: ", taskName)
  }
  if(rowNumA >= 1 && rowNumA <= nrow(meta$items) &&
     rowNumB >= 1 && rowNumB <= nrow(meta$items)) {
    if(rowNumA > rowNumB) {
      rows <- c(0:(rowNumB-1), rowNumA, (rowNumB+1):(rowNumA-1), rowNumB, (rowNumA+1):(nrow(meta$items)+1))
    } else {
      rows <- c(0:(rowNumA-1), rowNumB, (rowNumA+1):(rowNumB-1), rowNumA, (rowNumB+1):(nrow(meta$items)+1))
    }
    meta$items <- slice(meta$items, unique(rows))
  } else {
    stop("rowNum [", rowNumA, ", ", rowNumB, "] is invalid for task: ", taskName)
  }
  meta |> saveRDS(path)
  ## 支持管道定义
  taskName
}

#' @title 为任务增加函数任务脚本
#' @family task-script function
#' @export
script_func_add <- purrr::partial(script_item_add, type = "func")

#' @title 为任务增加字符串任务脚本
#' @family task-script function
#' @export
script_string_add <- purrr::partial(script_item_add, type = "string")

#' @title 为任务增加表达式任务脚本
#' @family task-script function
#' @export
script_expr_add <- purrr::partial(script_item_add, type = "expr")

#' @title 为任务增加文件任务脚本
#' @family task-script function
#' @export
script_file_add <- purrr::partial(script_item_add, type = "file")

#' @title 为任务增加目录任务脚本
#' @family task-script function
#' @export
script_dir_add <- purrr::partial(script_item_add, type = "dir")

#' @title 增加设置全局变量任务脚本
#' @family task-script function
#' @export
script_var_add <- purrr::partial(script_item_add, type = "var")

#' @title 更新为函数任务脚本
#' @family task-script function
#' @export
script_func_update <- purrr::partial(script_item_update, type = "func")

#' @title 更新为字符串任务脚本
#' @family task-script function
#' @export
script_string_update <- purrr::partial(script_item_update, type = "string")

#' @title 更新为表达式任务脚本
#' @family task-script function
#' @export
script_expr_update <- purrr::partial(script_item_update, type = "expr")

#' @title 更新为文件任务脚本
#' @family task-script function
#' @export
script_file_update <- purrr::partial(script_item_update, type = "file")

#' @title 更新为目录任务脚本
#' @family task-script function
#' @export
script_dir_update <- purrr::partial(script_item_update, type = "dir")

#' @title 更新为设置全局变量任务脚本
#' @family task-script function
#' @export
script_var_update <- purrr::partial(script_item_update, type = "var")
