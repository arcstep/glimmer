#' @title 查看数据集元数据
#' @param dsName 数据集名称
#' @param topic 主题域
#' @family metadata function
#' @export
ds_yaml <- function(dsName, topic = "CACHE") {
  path <- get_path(topic, dsName, ".metadata.yml")
  if(fs::file_exists(path)) {
    yaml::read_yaml(path)
  } else {
    list()
  }
}

#' @title 写入数据集的Yaml配置
#' @description 支持补写元数据的配置项
#' @param dsName 数据集名称
#' @param meta 主元数据内容
#' @param ex 扩展的元数据内容
#' @param data 样本数据，用来分析数据结构
#' @param topic 数据集保存的主题目录，默认为CACHE
#' @param type 数据集类型
#' @family metadata function
#' @export
ds_yaml_write <- function(dsName, meta = c(), ex = c(),  data = tibble(), topic = "CACHE", type = "__UNKNOWN__") {
  ## 自动创建数据集文件夹
  get_path(topic, dsName) |> fs::dir_create()
  
  ## 读取原有的元数据
  datasetMeta <- ds_yaml(dsName, topic)

  ## 自动生成额外的元数据项
  updateTimestamp <- lubridate::now()
  datasetMeta$datasetId <- digest::digest(fs::path_join(c(topic, dsName)), algo = "xxhash32")
  datasetMeta$topic <- topic
  datasetMeta$name <- dsName
  datasetMeta$type <- type
  datasetMeta$updateTime <- updateTimestamp |> as.integer()
  datasetMeta$updateTimeDesc <- lubridate::as_datetime(updateTimestamp, tz = "Asia/Shanghai") |> as.character()

  ## 使用meta参数对元数据做局部覆盖
  ## 部分名称在meta和ex中不允许被使用
  names(meta) |>
    purrr::walk(function(i) {
      if(i %nin% c("datasetId", "name", "topic", "type", "updateTime", "updateTimeDesc")) {
        datasetMeta[[i]] <<- meta[[i]]
      } else {
        warning(i, "is a built-in Keyword!!!")
      }
    })
  names(ex) |>
    purrr::walk(function(i) {
      if(i %nin% c("datasetId", "name", "topic", "type", "updateTime", "updateTimeDesc")) {
        datasetMeta[[i]] <<- ex[[i]]
      } else {
        warning(i, "is a built-in Keyword!!!")
      }
    })
  
  ## 如果元数据没有提供架构描述，则从样本数据中推断
  if(rlang::is_empty(meta$schema)) {
    if(!rlang::is_empty(data)) {
      datasetMeta$schema <- ds_schema(data) |>
        purrr::pmap(function(fieldType, fieldName) {
          list("fieldName" = fieldName, "fieldType" = fieldType)
        })
    }}

  ## 保存YAML文件
  yaml::write_yaml(datasetMeta, get_path(topic, dsName, ".metadata.yml"))
}

#' @title 将yaml配置中的架构转化为数据框
#' @family metadata function
#' @export
ds_yaml_schema <- function(dsName, topic = "CACHE") {
  yml <- ds_yaml(dsName, topic)
  yml$schema |>
    purrr::map_df(function(i) {
      list("fieldName" = i$fieldName, "fieldType" = i$fieldType)
    })
}