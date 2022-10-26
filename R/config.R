## 保存任务管理目录
TASK.ENV <- new.env(hash = TRUE)
## 保存schema对象
SCHEMA.ENV <- new.env(hash = TRUE)

## 构造访问路径-------

#' @title 根据配置构造数据路径
#' @param topic 任务主题
#' @family config functions
#' @export
get_path <- function(topic, ...) {
  p <- list(...)
  fs::path_join(c(get_topic(topic), unlist(p)))
}

## 获取内存配置项-------

#' @title 获取配置项的值
#' @param topic 任务主题
#' @family config functions
#' @export
get_topic <- function(topic) {
  get(topic, envir = TASK.ENV)
}

#' @title 列举所有配置项
#' @family config functions
#' @export
get_topics <- function() {
  ls(envir = TASK.ENV)
}

## 操作内存配置项-------
#' @title 获取所有配置
get_config <- function() {
  as.list(TASK.ENV)
}

#' @title 设置配置项的值
set_topic <- function(topic, path) {
  assign(topic, path, envir = TASK.ENV)
}

## 读写配置文件-----

#' @title 加载配置文件到内存
#' @description
#' 配置文件存在时，加载配置文件到运行环境。
#' 
#' ... >> ... >> 加载配置项
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
  ## 加载配置到内存
  topics <- config_yaml(path, yml)
  if("ROOT_PATH" %in% names(topics)) {
    if(!fs::dir_exists(topics$ROOT_PATH)) {
      fs::dir_create(topics$ROOT_PATH)
    }
    root_path <- topics[["ROOT_PATH"]]
  } else {
    root_path <- NULL
  }
  
  ## 设置主题并自动创建主题目录
  names(topics) |> purrr::walk(function(item) {
    if(item != "ROOT_PATH") {
      if(stringr::str_detect(topics[[item]], "^(\\.\\/)")) {
        set_topic(item, fs::path_join(c(root_path, topics[[item]])) |> fs::path_abs())
      } else {
        set_topic(item, topics[[item]])
      }
      get_path(item) |> fs::dir_create()
    }
  })
  
  ## 加载扩展的函数
  get_path("FUNS_DEFINE") |>
    fs::dir_ls(recurse = T, glob = "*.R", type = "file") |>
    purrr::walk(function(path) source(path))
  
  ## 加载扩展的函数定义元数据
  get_path("FUNS_DEFINE") |>
    fs::dir_ls(recurse = T, glob = "*.yml", type = "file") |>
    purrr::walk(function(p) {
      yml <- yaml::read_yaml(p)
      names(yml) |> purrr::walk(function(item) {
        assign(item, yml[[item]], envir = SCHEMA.ENV)
      })
    })
}

#' @title 创建或补写配置项到磁盘
#' @description
#' 多次运行时会增量补充。
#' 
#' ... >> 写入配置项 >> 加载配置项
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
        "SNAP" = "./SNAP",
        "IMPORT" = "./IMPORT",
        "CACHE" = "./CACHE",
        "TASK_SCRIPTS" = "./TASK_SCRIPTS",
        "TASK_DEFINE" = "./TASK_DEFINE",
        "FUNS_DEFINE" = "./FUNS_DEFINE")
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
#' @description
#' 自动创建\code{ROOT_PATH}目录。
#' 
#' 初始化配置文件夹 >> 写入配置项 >> 加载配置项
#' 
#' 如果配置文件路径已经存在，则使用已有的配置文件。
#' 
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
#' @description 
#' 读入配置文件，但暂不加载。
#' @param path YAML配置文件目录
#' @param yml 默认为config.yml
#' @family config functions
#' @export
config_yaml <- function(path = "./", yml = "config.yml") {
  yaml::read_yaml(fs::path_join(c(path, yml)))
}

## 加载预定义的函数定义元数据
fs::dir_ls("data/funs_schema", recurse = T, type = "file", glob = "*.yml") |>
  purrr::walk(function(p) {
    yml <- yaml::read_yaml(p)
    names(yml) |> purrr::walk(function(item) {
      assign(item, yml[[item]], envir = SCHEMA.ENV)
    })
  })

#' @title 查询函数Schema
#' @export
get_funs_schema <- function(..., entry = "funs_schema") {
  funs_schema <- as.list(SCHEMA.ENV)
  entryPath <- paste(entry, ..., sep = "$")
  item <- parse(text = entryPath) |> eval()
  ## 所选参数在plotly的schema定义中直接可以找到
  if("valType" %in% names(item)) {
    ## 直接返回非列表参数
    item
  } else {
    if(is.null(attr(item, "names"))) {
      list(
        "valType" = "array",
        "description" = "-",
        "items" = item,
        "entry" = entryPath)
    } else {
      list(
        "valType" = "object",
        "description" = "-",
        "items" = names(item),
        "entry" = entryPath)
    }
  }
}

#' @title 查询函数参数
#' @export
get_fun_schema <- function(funcName, ..., funcsTopic = NULL) {
  funcsTopic <- funcsTopic %empty% get_funs_schema()$items
  for(item in funcsTopic) {
    if(funcName %in% get_funs_schema(item, "functions")$items) {
      return(get_funs_schema(item, "functions", funcName, ...))
    }
  }
  return(list())
}

#' @title 查询ds函数
#' @export
get_ds_funs <- purrr::partial(get_funs_schema, entry = "funs_schema$ds$functions")
