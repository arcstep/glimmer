## 状态日志保存在ApacheParquet列存文件中
##
## 写时：按月保存Parquet文件
## 读时：直接读取Parquet文件集
##

#' @title 写入状态信息
#' @description 状态信息一定保存在STATE主题的目录下：{STATE}/{taskName}
#' @param taskName 任务名称，一般为“导入“，”构建“，”上传“等
#' @param title 子主题名称，可以是导入文件夹或某日构建等，也可以区分取数任务和取数明细
#' @param datasetName 数据集名称
#' @param rowID 数据行ID，一般为唯一关键列，可用于比对数据是否处理过等
#' @param flag 状态标志，方便读取状态信息时过滤
#' @param info 状态信息的详细描述
#' @details 
#' 应注意状态记录并非覆盖写入，而是追加，且一般不做删除或修改
#' 有明确约定的状态数据包括：
#' 
#' 1、标记处理完成的任务文件夹时，stateName = "__TASK_FOLDER__"
#' 2、写入parquet数据集文件时，stateName = "__WRITE_DATASET__"
#' 
#' @export
write_state <- function(stateName, dataNew) {
  ##
  lastModified <- lubridate::now()
  year <- lastModified |> lubridate::year()
  month <- lastModified |> lubridate::month()
  ## 如果文件夹不存在，那就创建出来
  confirm_STATE()
  p <- get_path("STATE", stateName, paste0("year=", year), paste0("month=", month))
  if(!fs::dir_exists(p)) {
    fs::dir_create(p)
  }
  ##
  d_new <- dataNew |> mutate(lastModified = lastModified)
  ##
  f <- fs::path_join(c(p, "data.parquet"))
  if(fs::file_exists(f)) {
    d_old <- arrow::read_parquet(f)
    rbind(d_old, d_new) |> arrow::write_parquet(f, version = "2.0")
  } else {
    d_new |> arrow::write_parquet(f, version = "2.0")
  }
}

#' @title 读取状态信息
#' @param stateName 状态数据名称
#' @param title 子主题名称
#' @export
read_state <- function(stateName) {
  confirm_STATE()
  p <- get_path("STATE", stateName)
  if(fs::dir_exists(p)) {
    arrow::open_dataset(p) |> arrange(desc(lastModified))
  } else {
    tibble()
  }
}

##
confirm_STATE <- function() {
  if(!("STATE" %in% get_topics())) stop("需要先使用'set_topic()'函数指定STATE文件夹")
}
