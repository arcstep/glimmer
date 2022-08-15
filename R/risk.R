
#' @title 初始化风险模型
#' @param modelName 模型名称
#' @param dataset 筛查目标数据集（该数据集必须存在yml配置文件，且设置了关键列）
#' @param filter 筛查条件
#' @param modelGroup 模型组
#' @param modelDesc 模型描述
#' @param author 模型作者
#' @param online 是否启用
#' @param overwrite 覆盖旧有模型
#' @param topic 风险模型的主题域
#' @family risk function
#' @export
risk_model_create <- function(
    modelName,
    dataset,
    filter = list(list(
      "column" = "col_name1",
      "op" = ">",
      "value" = 0:1,
      "riskTip" = "-",
      "level" = 1)),
    modelGroup = modelName,
    modelDesc = "-",
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
      "modelDesc" = modelDesc,
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
risk_model_all <- function(topic = "RISKMODEL") {
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

#' @title 读取风险模型
#' @param topic 主题名称
#' @family risk function
#' @export
risk_model_read <- function(modelName, topic = "RISKMODEL") {
  confirm_RISKMODEL(topic)
  path <- get_path(topic, modelName)
  if(fs::file_exists(paste0(path, ".yml"))) {
    yaml::read_yaml(paste0(path, ".yml"))
  } else {
    list()
  }
}

##
confirm_RISKMODEL <- function(topic = "RISKMODEL") {
  if(!(topic %in% get_topics()))
    stop("需要先使用'set_topic()'函数设置RISKMODEL文件夹")
}

#' @title 执行模型
#' @description 输出风险疑点数据到CACHE目录中
#' @param dsName 疑点数据集名称
#' @param targetTopic 疑点数据集主题域
#' @param topic 主题名称
#' @param verify 启用验证模式（不写入模型数据）
#' @family risk function
#' @export
risk_model_run <- function(modelName, dsName = "疑点数据", targetTopic = "CACHE", topic = "RISKMODEL", veryfy = FALSE) {
  item <- risk_model_read(modelName = modelName, topic = topic)
  if(rlang::is_empty(item)) {
    stop("Risk Model Not Existing: ", modelName)
  }
  
  yml <- ds_yaml(item$dataset)
  if(rlang::is_empty(yml$keyColumns)) {
    stop("Risk Model Config Invalid: ", modelName, " / ", "No KeyColumns in dataset ", item$dataset)
  }
  if(rlang::is_empty(yml$titleColumn)) {
    stop("Risk Model Config Invalid: ", modelName, " / ", "No TitleColumn in dataset ", item$dataset)
  }
  
  keyColumns <- yml$keyColumns |> stringr::str_split(",")
  
  d <- tibble()
  d_fact <- item$dataset |> ds_read(topic = targetTopic) |>
    select(!!!syms(keyColumns), !!!syms(yml$titleColumn))
  if(length(item$filter) > 0) {
    seq(1:length(item$filter)) |> purrr::walk(function(j) {
      message("Risk Model Runing: modelName")
      column <- item$filter[[j]]$column
      op <- item$filter[[j]]$op
      value <- item$filter[[j]]$value
      level <- item$filter[[j]]$level
      tip <- item$filter[[j]]$riskTip
      if(op %in% c(">", "<", ">=", "<=", "==", "!=")) {
        d <<- rbind(
          d,
          d_fact |>
            filter(do.call(!!sym(op), args = list(!!sym(column), value[[1]]))) |>
            collect() |>
            mutate("@level" = level, "@riskTip" = tip) |>
            unite("@value", !!!syms(column), sep = "--", remove = FALSE)
          )
      } else {
        stop("Risk Model ", item$modelName, "Unknown OP", op)
      }
    })
    
    if(!veryfy && nrow(d) > 0) {
      ## 执行模型并生成疑点数据
      d |>
        unite("@dataId", !!!syms(keyColumns), sep = "--") |>
        unite("@dataTitle", !!!syms(yml$titleColumn), sep = "--") |>
        select(`@dataId`, `@dataTitle`, `@level`, `@value`, `@riskTip`) |>
        rename(
          dataId = `@dataId`,
          dataTitle = `@dataTitle`,
          riskLevel = `@level`,
          riskTip = `@riskTip`,
          value = `@value`) |>
        mutate(
          modelName = modelName,
          dataset = item$dataset,
          desc = item$desc,
          modelGroup = item$modelGroup) |>
        
        ds_write(dsName = dsName, topic = targetTopic)
    } else {
      ## 启用验证模式
      message("Risk Model ", item$modelName, " OK !!")
    }
  }
}

#' @title 查看疑点数据
#' @param dsName 疑点数据集名称
#' @param targetTopic 疑点数据集主题域
#' @family risk function
#' @export
risk_data_read <- function(dsName = "疑点数据", targetTopic = "CACHE") {
  ds_read(dsName = dsName, topic = targetTopic)
}
  
  