## 保存任务管理目录
TASK.ENV <- new.env(hash = TRUE)

#' @title 获取配置
#' @param topic 任务主题
#' @family config functions
#' @export
get_topic <- function(topic) {
  get(topic, envir = TASK.ENV)
}

#' @title 获得所有主题
#' @family config functions
#' @export
get_topics <- function() {
  ls(envir = TASK.ENV)
}

#' @title 根据配置构造数据路径
#' @param topic 任务主题
#' @family config functions
#' @export
get_path <- function(topic, ...) {
  p <- list(...)
  fs::path_join(c(get_topic(topic), unlist(p)))
}

#' @title 设置主题目录
#' @param topic 任务主题
#' @param path 文件夹位置
#' @family config functions
#' @export
set_topic <- function(topic, path) {
  assign(topic, path, envir = TASK.ENV)
}

#' @title 加载配置文件
#' @description 批量执行\code{set_topic}任务
#' @details 
#' 使用\code{ROOT_PATH}时有一个关键约定：
#' 配置项必须以\code{./}开头，才能使用\code{ROOT_PATH}扩展其路径；
#' 否则，将被视为独立配置名。
#' 
#' @param yml_file YAML配置文件
#' @family config functions
#' @export
load_config <- function(yml_file) {
  topics <- yaml::read_yaml(yml_file)
  if("ROOT_PATH" %in% names(topics)) {
    root_path <- topics[["ROOT_PATH"]]
  } else {
    root_path <- NULL
  }
  names(topics) |> purrr::walk(function(item) {
    if(item != "ROOT_PATH") {
      if(stringr::str_detect(topics[[item]], "^(\\.\\/)")) {
        set_topic(item, fs::path_join(c(root_path, topics[[item]])) |> fs::path_abs())
      } else {
        set_topic(item, topics[[item]])
      }
    }
  })
}

#' @title 读取所有配置项
#' @family config functions
#' @export
get_config <- function() {
  as.list(TASK.ENV)
}