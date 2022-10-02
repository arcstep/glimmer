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
#' @description 允许对配置文件做局部更新
#' @param meta 元数据内容，支持局部更新
#' @param dsName 数据集名称
#' @param topic 数据集保存的主题目录，默认为CACHE
#' @family metadata function
#' @export
# "desc" = desc,
# "nrow" = nrow(d),
# "columns" = names(d),
# "partColumns" = partColumns,
# "keyColumns" = keyColumns,
# "suggestedColumns" = suggestedColumns,
# "titleColumn" = titleColumn,
# "lastUpdate" = updated,
# "lastAffected" = affectedParts$path

ds_write_yaml <- function(meta, dsName, topic = "CACHE") {
  ## confirm dataset folder exist
  get_path(topic, dsName) |> fs::dir_create()
  datasetMeta <- ds_yaml(dsName, topic)

  ## write meta data with partition
  names(meta) |>
    purrr::walk(function(i) {
      datasetMeta[[i]] <<- meta[[i]]
    })
  ## auto generate items
  updateTimestamp <- lubridate::now(tz = "Asia/Shanghai")
  datasetMeta$datasetId <- digest::digest(fs::path_join(c(topic, dsName)), algo = "xxhash32")
  datasetMeta$topic <- topic
  datasetMeta$name <- dsName
  datasetMeta$updateTime <- updateTimestamp |> as.integer()
  datasetMeta$updateTimeDesc <- lubridate::as_datetime(updateTimestamp, tz = "Asia/Shanghai") |> as.character()
  ## save yaml
  yaml::write_yaml(datasetMeta, get_path(topic, dsName, ".metadata.yml"))
}