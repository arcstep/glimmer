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
      get_path(item) |> fs::dir_create()
    }
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

## 获取函数列表
get_funcs <- function(prefix = "^.") {
  x <- lsf.str("package:glimmer")
  x[x |> stringr::str_detect(prefix)]
}

#' @title 获取函数参数
#' @description 根据规则生成参数要求
#' 
#' 如果参数命名格式按照如下规范约定，则自动生成参数要求，例如：
#' \itemize{
#' \item s_ 要求单个字符串
#' \item i_ 要求整数
#' \item f_ 要求浮点数
#' \item t_ 要求时间戳整数
#' \item c_ 颜色枚举
#' \item b_ 要求布尔类型
#' 可以是静态文件定义，也可以是从某个数据集中实时提取
#' }
#' @param funcName 函数名称，可以是字符串或函数名
#' @export
get_params <- function(funcName) {
  p <- tribble(
    ~prefix, ~typeName, ~tips,
    "s_", "string", "字符串",
    "sv_", "n_string", "字符串序列",
    "i_", "int", "整数",
    "iv_", "int", "整数序列",
    "f_", "float", "浮点数",
    "fv_", "float", "数值序列",
    "ds_", "date_string", "日期字符串",
    "dts_", "datetime_string", "日期时间字符串",
    "ts_", "time_string", "时间字符串",
    "t_", "timestamp", "时间戳",
    "c_", "color", "颜色",
    "b_", "bool", "布尔",
    "e_", "enum", "枚举",
    "o_", "operater", "逻辑操作符"
  )
  tibble(paramName = formalArgs(funcName)) |>
    mutate(prefix = stringr::str_remove(paramName, "(?<=_)(.*)")) |>
    left_join(p, by = "prefix")
}

#' @title 所有gali函数
#' @export
get_funs_gali <- function() {
  p <- tribble(
    ~midfix, ~input, ~output, ~tips,
    "import", "-", "tibble", "导入各类数据",
    "export", "tibble", "-", "导出各类数据或报表",
    "read", "dataset", "tibble", "读取Parquet文件组数据集",
    "save", "tibble", "parquet", "保存Parquet文件组数据集",
    "dataset", "tibble", "tibble", "支持管道的数据框处理",
    "plotly", "tibble", "plotly", "绘制plotly图表",
    "trace", "plotly", "plotly", "增加plotly绘制层",
    "DT", "tibble", "DT", "绘制DT数据表"
  )
  x <- lsf.str("package:glimmer")
  tibble(funcName = x[x |> stringr::str_detect("^gali_")]) |>
    mutate(midfix = stringr::str_remove(funcName, "gali_") |> stringr::str_remove("(?<=)_.+")) |>
    left_join(p, by = "midfix")
}
