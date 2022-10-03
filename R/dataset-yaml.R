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
#' @param meta 元数据内容，支持局部更新
#' @param dsName 数据集名称
#' @param data 样本数据，用来分析数据结构
#' @param topic 数据集保存的主题目录，默认为CACHE
#' @family metadata function
#' @export
ds_yaml_write <- function(meta, dsName, data = tibble(), topic = "CACHE") {
  ## 自动创建数据集文件夹
  get_path(topic, dsName) |> fs::dir_create()
  
  ## 读取原有的元数据
  datasetMeta <- ds_yaml(dsName, topic)

  ## 自动生成额外的元数据项
  updateTimestamp <- lubridate::now(tz = "Asia/Shanghai")
  datasetMeta$datasetId <- digest::digest(fs::path_join(c(topic, dsName)), algo = "xxhash32")
  datasetMeta$topic <- topic
  datasetMeta$name <- dsName
  datasetMeta$updateTime <- updateTimestamp |> as.integer()
  datasetMeta$updateTimeDesc <- lubridate::as_datetime(updateTimestamp, tz = "Asia/Shanghai") |> as.character()

  ## 使用meta参数对元数据做局部覆盖
  names(meta) |>
    purrr::walk(function(i) {
      datasetMeta[[i]] <<- meta[[i]]
    })
  
  ## 如果元数据没有提供架构描述，则从样本数据中推断
  if(rlang::is_empty(meta$schema)) {
    datasetMeta$schema <- ds_schema(data) |>
      purrr::pmap(function(fieldType, fieldName) {
        list("name" = fieldName, "type" = fieldType)
      })
  }

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
      list(fieldName = i$name, fieldType = i$type)
    })
}