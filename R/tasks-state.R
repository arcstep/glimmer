## 状态日志保存在ApacheParquet列存文件中
##
## 写时：按月保存Parquet文件
## 读时：直接读取Parquet文件集
##

#' @title 写入数据到状态数据集
#' @description 状态数据集一定保存在STATE主题的目录下：{STATE}/{stateName}
#' @details 
#' 状态数据集保存在STATE主题的目录下：{STATE}/{stateName}
#' 按写入时间的年和月建立分区
#' 操作状态数据集是消耗IO的操作，需要对分区文件频繁覆写
#' 
#' 状态数据集的内容是通过传入的数据框（dataNew）扩展的，
#' 管理状态数据集的模块，要自己负责保障状态数据集结构不变
#' 
#' @param stateName 状态数据集名称，存储时使用该名称构造路径，允许带斜线
#' @param dataNew 保存状态内容的数据框，可以根据状态数据集扩展要保存的列
#' @details 
#' 应注意状态记录主要是追加写入，一般不做删除或修改
#' 以下是有明确约定的状态数据集名称：
#' 
#' 1、标记处理完成的任务文件夹时，stateName = "__TASK_FOLDER__"
#' 2、写入parquet数据集文件时，stateName = "__WRITE_DATASET__"
#' 
#' @export
write_state <- function(stateName, tibbleNew, topic = "STATE") {
  ##
  lastModified <- lubridate::now()
  year <- lastModified |> lubridate::year()
  month <- lastModified |> lubridate::month()
  
  ## 如果文件夹不存在，那就创建出来
  confirm_STATE()
  p <- get_path(topic, stateName, paste0("year=", year), paste0("month=", month))
  if(!fs::dir_exists(p)) {
    fs::dir_create(p)
  }

  ##
  d_new <- tibbleNew |> mutate(lastModified = lastModified)
  
  ## 按HIVE风格写入特定分区
  f <- fs::path_join(c(p, "data.parquet"))
  if(fs::file_exists(f)) {
    d_old <- arrow::read_parquet(f)
    rbind(d_old, d_new) |> arrow::write_parquet(f, version = "2.0")
  } else {
    d_new |> arrow::write_parquet(f, version = "2.0")
  }
  
  ## 更新元数据集元件
  d <- arrow::open_dataset(get_path(topic, stateName), format = "parquet")
  datasetMeta <- list(
    "datasetId" = digest::digest(fs::path_join(c(topic, stateName)), algo = "xxhash32"),
    "topic" = topic,
    "name" = stateName,
    "desc" = "-",
    "columns" = names(d) |> paste(collapse = ","),
    "rows" = nrow(d),
    "partColumns" = "year, month",
    "keyColumns" = "",
    "updateAt" = lubridate::as_datetime(lastModified, tz = "Asia/Shanghai") |> as.character(),
    "updateTime" = lastModified |> as.integer(),
    "lastUpdate" = "-",
    "lastAffected" = "-"
  )
  yaml::write_yaml(datasetMeta, get_path(topic, stateName, ".metadata.yml"))
}

#' @title 读取状态信息
#' @param stateName 状态数据集名称
#' @export
read_state <- function(stateName, topic = "STATE") {
  confirm_STATE(topic)
  p <- get_path(topic, stateName)
  if(fs::dir_exists(p)) {
    arrow::open_dataset(p) |> arrange(desc(lastModified))
  } else {
    tibble()
  }
}

##
confirm_STATE <- function(topic = "STATE") {
  if(!(topic %in% get_topics())) stop("需要先使用'set_topic()'函数指定STATE文件夹")
}
