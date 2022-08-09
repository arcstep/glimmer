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

#' @title 查看执行计划
#' @description 按执行顺序罗列需要执行的脚本
#' @param path 文件夹位置
#' @param glob 要执行的源文件默认以.R结尾
#' @family TaskFolder functions
#' @export
get_task_plan <- function(path, glob = "*.R") {
  fs::dir_ls(path, recurse = T, glob = glob, type = "file") |>
    purrr::map_df(function(item) {
      name <- fs::path_file(item)
      list("name" = name, "path" = item)
    })
}

#' @title 执行目标路径下的任务脚本
#' @description 应当按照脚本顺序执行
#' @family TaskFolder functions
#' @export
run_task <- function(path, title = "-") {
  get_task_plan(path) |> purrr::pmap(function(name, path) {
    message("执行任务：", path)
    beginTime <- lubridate::now()
    source(path)
    topic <- lubridate::now() |> lubridate::as_date()
    msg <- paste0("任务耗时：", as.character.Date(lubridate::now() - beginTime))
    write_state(taskName = "RUN_TASK", title = title, flag = path, detail = msg)
    message(msg)
  })
}
