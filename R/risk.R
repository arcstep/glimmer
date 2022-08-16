
#' @title 初始化风险模型
#' @description 按照指定列的阈值条件过滤风险数据
#' @details 
#' 1、风险模型应以模型名为关键列，模型名中可携带版本号
#' 2、已生成数据的过期风险模型不应直接删除，而应注意停用，停用时应清理过期数据
#' 3、要支持不同级别的风险数据，应设置多个风险模型，但可设置为同一模型组
#' 4、同一个风险模型的多个分支也可以设置为同一组
#' 
#' @param modelName 模型名称
#' @param dataset 筛查目标数据集（该数据集必须存在yml配置文件，且设置了关键列）
#' @param riskTip 风险提示说明
#' @param level 风险级别，L为低风险，M为中风险，H为高风险
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
    riskTip = "-",
    level = "L",
    filter = list(list(
      "column" = "col_name1",
      "op" = ">",
      "value" = 0:1)),
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
      "riskTip" = riskTip,
      "level" = level,
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
#' @param modelName 模型名称
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
#' @param modelName 模型名称
#' @param batchNumber 批次号
#' @param dsName 疑点数据集名称
#' @param targetTopic 疑点数据集主题域
#' @param topic 主题名称
#' @param verify 启用验证模式（不写入模型数据）
#' @family risk function
#' @export
risk_model_run <- function(
    modelName,
    batchNumber = lubridate::now(tz = "Asia/Shanghai") |> as.integer(),
    dsName = "疑点数据",
    targetTopic = "CACHE", topic = "RISKMODEL", veryfy = FALSE) {
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
  
  d <- item$dataset |> ds_read(topic = targetTopic)
  if(length(item$filter) > 0) {
    seq(1:length(item$filter)) |> purrr::walk(function(j) {
      message("Risk Model Runing: modelName")
      column <- item$filter[[j]]$column
      op <- item$filter[[j]]$op
      value <- item$filter[[j]]$value
      if(op %in% c(">", "<", ">=", "<=", "==", "!=", "%in%")) {
        d <<- d |>
          filter(do.call(
            !!sym(op),
            args = list(!!sym(column), unlist(value)))) |>
          collect()
      } else if(stringr::str_detect(op, "%time%")) {
        myop <- stringr::str_replace(op, "%time%", "")
        d <<- d |>
          filter(do.call(
            !!sym(myop),
            args = list(!!sym(column), unlist(value) |> lubridate::as_datetime(tz = "Asia/Shanghai")))) |>
          collect()
      } else if(stringr::str_detect(op, "%date%")) {
        myop <- stringr::str_replace(op, "%date%", "")
        d <<- d |>
          filter(do.call(
            !!sym(myop),
            args = list(!!sym(column), unlist(value) |> lubridate::as_date(tz = "Asia/Shanghai")))) |>
          collect()
      } else if(op %in% c("%nin%")) {
        ## 将 %nin% 转换为可以惰性执行的 %in%
        d <<- d |>
          filter(!do.call(
            "%in%",
            args = list(!!sym(column), unlist(value)))) |>
          collect()
      } else if(op %in% c("%regex%", "%not-regex%")) {
        ## 正则表达式需要不能惰性执行，需要提前collect数据
        d <<- d |>
          collect() |>
          filter(do.call(
            !!sym(op),
            args = list(!!sym(column), unlist(value))))
      } else {
        stop("Risk Model: ", item$modelName, " >> Unknown OP: ", op)
      }
    })
    
    if(!veryfy && nrow(d) > 0) {
      ## 执行模型并生成疑点数据
      columns <- item$filte |> lapply(function(item) item$column) |> unlist()
      d |>
        unite("@dataId", !!!syms(keyColumns), sep = "--", remove = FALSE) |>
        unite("@dataTitle", !!!syms(yml$titleColumn), sep = "--", remove = FALSE) |>
        unite("@value", !!!syms(columns), sep = ",", remove = FALSE) |>
        select(`@dataId`, `@dataTitle`, `@value`) |>
        rename(
          dataId = `@dataId`,
          dataTitle = `@dataTitle`,
          value = `@value`) |>
        mutate(
          riskLevel =  item$level,
          riskTip = item$riskTip,
          modelName = modelName,
          batchNumber = batchNumber,
          runAt = lubridate::now(tz = "Asia/Shanghai"),
          dataset = item$dataset,
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
  
  