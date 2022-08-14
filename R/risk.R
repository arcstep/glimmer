
#' @title 初始化风险模型
#' @family risk function
#' @export
risk_model_create <- function(
    modelName,
    dataset,
    filter = list(list("column" = "col_name1", "op" = ">", "value" = 0:1)),
    modelGroup = modelName,
    desc = "-",
    author = "-",
    online = FALSE,
    overwrite = FALSE,
    topic = "RISKMODEL") {
  createdAt <- lubridate::now(tz = "Asia/Shanghai")
  lastModified <- lubridate::now(tz = "Asia/Shanghai")
  
  confirm_RISKMODEL(topic)
  
  path <- get_path(topic, paste0(modelName, ".yml"))
  path_dir <- fs::path_dir(path)
  if(!fs::dir_exists(path_dir)) {
    fs::dir_create(path_dir)
  }
  
  if(!fs::file_exists(path) || overwrite) {
    yml <- list(
      "modelName" = modelName,
      "dataset" = dataset,
      "filter" = filter,
      "modelGroup" = modelGroup,
      "desc" = desc,
      "author" = author,
      "online" = online,
      "createdAt" = createdAt |> as.character.Date(),
      "lastModified" = lastModified |> as.character.Date()
    )
    yml |> yaml::write_yaml(path)
    message("Risk Model Created: ", modelName)
  } else {
    warning("Risk Model Existing: ", modelName)
  }
}

#' @title 读取风险模型
#' @param topic 主题名称
#' @family risk function
#' @export
risk_model_read <- function(topic = "RISKMODEL") {
  confirm_RISKMODEL(topic)
  path <- get_path(topic)
  if(fs::dir_exists(path)) {
    fs::dir_ls(path, type = "file", all = T, glob = "*.yml", recurse = T) |>
      purrr::map_df(function(path) {
        yaml::read_yaml(path)
      })
  } else {
    tibble()
  }
}

##
confirm_RISKMODEL <- function(topic = "RISKMODEL") {
  if(!(topic %in% get_topics())) stop("需要先使用'set_topic()'函数设置RISKMODEL文件夹")
}

#' @title 执行模型
#' @description 输出风险疑点数据到CACHE目录中
#' @family risk function
#' @export
risk_model_run <- function(dataset = "疑点数据", targetTopic = "CACHE", topic = "RISKMODEL") {
  
  ds_write(dsName = dataset, topic = targetTopic)
}

#' @title 查看疑点数据
#' @family risk function
#' @export
risk_data_read <- function(dataset = "疑点数据", targetTopic = "CACHE") {
  ds_read(dsName = dataset, topic = targetTopic)
}
  
  