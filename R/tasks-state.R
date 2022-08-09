## 状态日志保存在ApacheParquet列存文件中
##
## 写时：按月保存Parquet文件
## 读时：直接读取Parquet文件集
##

#' @title 写入状态信息
#' @description 状态信息一定保存在STATE主题的目录下：{STATE}/{taskName}/{subTopic}
#' @param taskName 任务名称，一般为“导入“，”构建“，”上传“等
#' @param subTopic 子主题名称，可以是导入文件夹或某日构建等，也可以区分取数任务和取数明细
#' @param datasetName 数据集名称
#' @param rowID 数据行ID，一般为唯一关键列，可用于比对数据是否处理过等
#' @param flag 状态标志，方便读取状态信息时过滤
#' @param info 状态信息的详细描述
#' @details 
#' 应注意状态记录并非覆盖写入，而是追加，且一般不做删除或修改
#' @export
write_state <- function(taskName, subTopic, datasetName = "-", rowID = "-", flag = "", detail = "") {
  ##
  lastModified <- lubridate::now()
  year <- lastModified |> lubridate::year()
  month <- lastModified |> lubridate::month()
  ## 如果文件夹不存在，那就创建出来
  confirm_STATE()
  p <- get_path("STATE", taskName, subTopic, paste0("year=", year), paste0("month=", month))
  if(!fs::dir_exists(p)) {
    fs::dir_create(p)
  }
  ##
  d_new <- tibble::tribble(
    ~lastModified, ~taskName, ~subTopic, ~datasetName, ~rowID, ~flag, ~detail,
    lastModified, taskName, subTopic, datasetName, rowID, flag, detail
  )
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
#' @param taskName 任务名称
#' @param subTopic 子主题名称
#' @export
read_state <- function(taskName, subTopic) {
  confirm_STATE()
  p <- get_path("STATE", taskName, subTopic)
  if(fs::dir_exists(p)) {
    arrow::open_dataset(p)
  } else {
    stop("要打开的数据文件夹不存在：", p)
  }
}

##
confirm_STATE <- function() {
  if(!("STATE" %in% get_topics())) stop("需要先使用'set_topic()'函数指定STATE文件夹")
}
