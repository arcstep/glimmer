## 保存任务管理目录
TASK.ENV <- new.env(hash = TRUE)

## 构造访问路径-------

#' @title 根据配置构造数据路径
#' @param topic 任务主题
#' @family config functions
#' @export
get_path <- function(topic, ...) {
  p <- list(...)
  fs::path_join(c(get_topic(topic), unlist(p)))
}

## 内存中配置项-------

#' @title 获取配置
#' @param topic 任务主题
#' @family config functions
#' @export
get_topic <- function(topic) get(topic, envir = TASK.ENV)

#' @title 获得所有主题
#' @family config functions
#' @export
get_topics <- function() ls(envir = TASK.ENV)

## 操作内存配置项-------
get_config <- function() as.list(TASK.ENV)
set_topic <- function(topic, path) assign(topic, path, envir = TASK.ENV)

## 读写配置文件-----

#' @title 加载配置文件到内存
#' @description
#' 自动创建\code{ROOT_PATH}目录。
#' 
#' 根据配置文件配置目录到运行环境。
#' @details 
#' 使用\code{ROOT_PATH}时有一个关键约定：
#' 配置项必须以\code{./}开头，才能使用\code{ROOT_PATH}扩展其路径；
#' 否则，将被视为独立配置名。
#' 
#' @param path 默认为配置文件所在的目录
#' @param yml 默认为config.yml
#' @family config functions
#' @export
config_load <- function(path = "./", yml = "config.yml") {
  topics <- config_yaml(path, yml)
  if("ROOT_PATH" %in% names(topics)) {
    if(!fs::dir_exists(topics$ROOT_PATH)) {
      fs::dir_create(topics$ROOT_PATH)
    }
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

#' @title 创建或补写配置项到磁盘
#' @description
#' 多次运行时会增量补充
#' @param path YAML配置文件目录
#' @param yml 默认为config.yml
#' @param option 配置项
#' @family config functions
#' @export
config_write <- function(path = "./", yml = "config.yml", option = list()) {
  if(fs::dir_exists(path)) {
    ## 写入配置
    if(fs::file_exists(fs::path_join(c(path, yml)))) {
      xoption <- config_yaml(path, yml)
    } else {
      xoption <- list(
        "ROOT_PATH" = fs::path_abs(path),
        "IMPORT" = "./IMPORT",
        "CACHE" = "./CACHE",
        "TASK_SCRIPTS" = "./TASK_SCRIPTS",
        "TASK_DEFINE" = "./TASK_DEFINE")
    }
    names(option) |> purrr::walk(function(i) {
      xoption[[i]] <<- option[[i]]
    })
    xoption |> yaml::write_yaml(fs::path_join(c(path, yml)))
    ## 加载配置
    config_load(path, yml)
    ## 返回内存中的所有配置
    get_config()
  } else {
    stop("Path Folder Not Exist: ", path)
  }
}
#' @title 初始化配置项
#' @description 初始化文件夹、配置项设定和加载
#' @param path YAML配置文件目录
#' @param yml 默认为config.yml
#' @param option 配置项
#' @family config functions
#' @export
config_init <- function(path = "./",
                        yml = "config.yml",
                        option = list()) {
  ## 创建配置文件目录
  if(!fs::dir_exists(path)) {
    fs::dir_create(path)
  }
  ## 写入配置
  config_write(path, yml, option)
}

#' @title 读取yaml配置文件为列表
#' @param path YAML配置文件目录
#' @param yml 默认为config.yml
#' @family config functions
#' @export
config_yaml <- function(path = "./", yml = "config.yml") {
  yaml::read_yaml(fs::path_join(c(path, yml)))
}