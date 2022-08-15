
#' @title 初始化风险模型
#' @param modelName 模型名称
#' @param dataset 筛查目标数据集（该数据集必须存在yml配置文件，且设置了关键列）
#' @param title 标题列
#' @param filter 筛查条件
#' @param template 生成疑点数据时使用的风险描述模板
#' @param modelGroup 模型组
#' @param desc 模型描述
#' @param author 模型作者
#' @param online 是否启用
#' @param overwrite 覆盖旧有模型
#' @param topic 风险模型的主题域
#' @family risk function
#' @export
risk_model_create <- function(
    modelName,
    dataset,
    title = NULL,
    template = NULL,
    filter = list(list("column" = "col_name1", "op" = ">", "value" = 0:1, "level" = 1)),
    modelGroup = modelName,
    desc = NULL,
    author = NULL,
    online = FALSE,
    overwrite = FALSE,
    topic = "RISKMODEL") {
  createdAt <- lubridate::now(tz = "Asia/Shanghai")
  lastModified <- lubridate::now(tz = "Asia/Shanghai")
  ds <- ds_read()

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
      "title" = title,
      "template" = template,
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
risk_model_run <- function(dsName = "疑点数据", targetTopic = "CACHE", topic = "RISKMODEL", ) {
  models <- risk_model_read(topic = topic)
  seq(1:nrow(models)) |> purrr::walk(function(i) {
    item <- slice(models, i) |> as.list()
    d <- item$dataset |> ds_read(topic = targetTopic)
    seq(1:length(item$filter)) |> purrr::walk(function(j) {
      column <- item$filter[[j]]$column
      op <- item$filter[[j]]$op
      value <- item$filter[[j]]$value
      if(op %in% c(">", "<", ">=", "<=", "==", "!=")) {
        d <<- d |> filter(do.call(!!sym(op), args = list(!!sym(column), value[[1]])))
      } else {
        warning("RISK MODEL", item$modelName, "UNKNOWN OP", op)
      }
    })
    d |> collect() |> ds_write(dsName = dsName, topic = targetTopic)
  })
}

#' @title 查看疑点数据
#' @family risk function
#' @export
risk_data_read <- function(dsName = "疑点数据", targetTopic = "CACHE") {
  ds_read(dsName = dsName, topic = targetTopic)
}
  
  