#' @title 写入ApachheParquet文件集
#' @param d 要写入的数据
#' @param dsName 数据集名称
#' @param topic 数据集保存的主题目录，默认为CACHE
#' @param partColumns 分区列，可以是字符串向量
#' @param keyColumns 关键列，可以是字符串向量
#' @param desc 对数据集的额外描述
#' @description 更新受影响的分区
#' @family dataset function
#' @details
#' 默认为更新模式；如果仅直接覆写数据，应先删除数据集
#' 
#' 如果不设置关键列，则为追加模式；否则按关键列替换
#' 
#' @export
ds_write <- function(d, dsName, topic = "CACHE", partColumns = c(), keyColumns = c(), desc = "-") {
  ## 默认从CACHE任务目录读写数据集
  path <- get_path(topic, dsName)
  
  ## 确定数据集不为空
  if(!is.data.frame(d)) stop("Not Tibble Object to write >>> ", path)
  if(rlang::is_empty(d)) {
    warning("Empty Dataset to write >>> ", path)
  } else {
    if(nrow(d)==0) {
      warning("No Content in New Dataset to write >>> ", path)
    }
  }
  
  ## 如果旧数据集已经存在
  affected_data <- tibble()
  
  if(fs::dir_exists(path)) {
    ## 确定与已存在数据集结构一致
    d_old <- arrow::open_dataset(path, format = "parquet") |> head() |> collect()
    diff_info <- d |> ds_diff_dataset(d_old) |> filter(!equal)
    if(nrow(diff_info) > 0) {
      print(diff_info)
      stop("Different Schema to write >> ", path)
    }
    
    ## 找出受影响的分区数据
    if(rlang::is_empty(partColumns)) {
      affected_data <- arrow::open_dataset(path, format = "parquet") |>
        collect()
    } else {
      if(rlang::is_empty(keyColumns)) {
        ## 如果指定关键列，就按关键列去重
        affected_data <- arrow::open_dataset(path, format = "parquet") |>
          semi_join(d, by = partColumns) |>
          collect()
      } else {
        ## 如果指定关键列，就按关键列去重
        affected_data <- arrow::open_dataset(path, format = "parquet") |>
          semi_join(d, by = partColumns) |>
          anti_join(d, by = keyColumns) |>
          collect()
      }
    }
  }
  
  ## 当写入数据到磁盘时
  if(nrow(d) > 0) {
    to_write <- affected_data |> rbind(d)
    beginTimestamp <- lubridate::now(tz = "Asia/Shanghai")
    # Sys.sleep(1)
    ## 写入分区
    arrow::write_dataset(
      to_write,
      path,
      format = "parquet",
      partitioning = partColumns,
      version = "2.0",
      existing_data_behavior = "delete_matching")
    
    ## 登记已写入分区状态，方便按分区增量变化做其他处理
    allPartsInfo <- fs::dir_ls(path, type = "file", recurse = T, glob = "*.parquet") |> fs::file_info()
    affectedParts <- allPartsInfo |> filter(modification_time > beginTimestamp)
    affected <- affectedParts$path |> paste(collapse = ",")
    updated <- paste("affected ", nrow(to_write), "rows", ",", nrow(affectedParts), "parts")
    message("write_dataset << ", dsName, " >> ", updated)
    state_write(
      stateName = "__WRITE_DATASET__",
      tibble(
        "dataset" = dsName,
        "updated" = updated,
        "affected" = affected
      ))
    
    ## 更新元数据集元件
    d <- arrow::open_dataset(path, format = "parquet")
    updateTimestamp <- lubridate::now(tz = "Asia/Shanghai")
    datasetMeta <- list(
      "datasetId" = digest::digest(fs::path_join(c(topic, dsName)), algo = "xxhash32"),
      "topic" = topic,
      "name" = dsName,
      "desc" = desc,
      "columns" = names(d) |> paste(collapse = ","),
      "rows" = nrow(d),
      "partColumns" = partColumns |> paste(collapse = ","),
      "keyColumns" = keyColumns |> paste(collapse = ","),
      "updateAt" = lubridate::as_datetime(updateTimestamp, tz = "Asia/Shanghai") |> as.character(),
      "updateTime" = updateTimestamp |> as.integer(),
      "lastUpdate" = updated,
      "lastAffected" = affected
    )
    yaml::write_yaml(datasetMeta, get_path(topic, dsName, ".metadata.yml"))
  }
}

#' @title 读取重写过分区的数据集文件
#' @param affectedParts 受影响的分区文件，多个时用逗号间隔表示
#' @family dataset function
#' @export
ds_read_affected <- function(affectedParts) {
  affectedParts |>
    stringr::str_split(",") |>
    unlist() |>
    arrow::open_dataset(format = "parquet")
}

#' @title 读取最近一次更新时重写过分区的数据集文件
#' @param dsName 数据集名称
#' @param stateTopic 状态数据主题域
#' @family dataset function
#' @export
ds_last_affected <- function(dsName = c(), stateTopic = "STATE") {
  if(rlang::is_empty(dsName)) {
    state <- state_read("__WRITE_DATASET__", stateTopic) |>
      arrange(desc(lastModified)) |> collect()
  } else {
    state <- state_read("__WRITE_DATASET__") |>
      filter(.data$dataset == dsName) |>
      arrange(desc(lastModified)) |> collect()
  }
  state$affected[[1]] |> ds_read_affected()
}

#' @title 读取数据集
#' @param dsName 数据集名称
#' @param topic 主题域
#' @family dataset function
#' @export
ds_read <- function(dsName, topic = "CACHE") {
  path <- get_path(topic, dsName)
  arrow::open_dataset(path, format = "parquet")
}

#' @title 列举所有数据集
#' @param topic 主题域
#' @family dataset function
#' @export
ds_all <- function(topic = "CACHE") {
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

#' @title 列举所有数据集
#' @param dsName 数据集名称
#' @param topic 主题域
#' @family dataset function
#' @export
ds_yaml <- function(dsName, topic = "CACHE") {
  path <- get_path(topic, dsName)
  if(fs::file_exists(paste0(path, "yml"))) {
    paste0(path, ".yml") |>
      yaml::read_yaml()
  } else {
    list()
  }
}

#' @title 移除数据文件夹
#' @description 可以整个移除，也可以组装目录后按分区移除
#' @param dsName 数据集名称
#' @param topic 主题域
#' @family dataset function
#' @export
ds_remove_path <- function(dsName, topic = "CACHE") {
  path <- get_path(topic, dsName)
  if(fs::dir_exists(path)) {
    fs::dir_delete(path)
    message("[REMOVED DIR] >>> ", path)
  } else {
    message("[Empty DIR] >>> ", path)
  }
}

#' @title 查看数据集架构
#' @description
#' 查看数据集架构时，必须包含数据
#' @param ds 数据框
#' @family dataset function
#' @export
ds_schema <- function(ds) {
  if(rlang::is_empty(ds)) {
    tibble()
  } else {
    tibble(
      fieldName = names(ds),
      fieldType = lapply(names(ds), function(field) {typeof(ds[[field]])}) |> unlist()
    ) |> arrange(.data[["fieldName"]])
  }
}

#' @title 确认数据集同构
#' @description
#' 比较两个数据集的列数、列名、列字段类型是否一致。
#' @details
#' 如果列结构完全相等，则返回列表中的equal为TRUE，否则为FALSE；
#' 如果第一个数据集中的列在第二个数据集中全部存在，且字段类型一致，则返回列表中的
#' contained为TRUE，否则为FALSE。
#'
#' 在需要时，可以将ds_schema获得的数据集结构持久化保存（例如，使用saveRDS），
#' 然后用于比较新导入、新生成的数据集是否合规。
#' 也可以直接使用 dataset_comfirm 来完成这个工作。
#' @param schema1 用于比较的数据集结构，可使用ds_schema获得
#' @param schema2 用于参考的数据集结构，可使用ds_schema获得
#' @family dataset function
#' @export
ds_compare_schema <- function(schema1, schema2) {
  if(rlang::is_empty(schema2)) {
    list("equal" = FALSE, "contained" = FALSE)
  } else {
    result <- ds_diff_schema(schema1, schema2)
    equal <- !purrr::some(result$equal, function(item) !item)
    contained <- !purrr::some(result$contained, function(item) !item)
    list(equal=equal, contained=contained)
  }
}

#' @title 详细比较两个数据集结构
#' @param schema1 用于比较的数据集结构，可使用\code{\link{ds_schema}}获得
#' @param schema2 用于参考的数据集结构，可使用\code{\link{ds_schema}}获得
#' @family dataset function
#' @export
ds_diff_schema <- function(schema1, schema2) {
  full_join(schema1, schema2, by = c("fieldName")) |>
    mutate(equal = .data[["fieldType.x"]]==.data[["fieldType.y"]]) |>
    mutate(equal = ifelse(is.na(equal), FALSE, equal)) |>
    mutate(contained = is.na(.data[["fieldType.x"]]) | .data[["fieldType.x"]]==.data[["fieldType.y"]]) |>
    mutate(contained = ifelse(is.na(.data[["contained"]]), FALSE, .data[["contained"]]))
}

#' @title 详细比较两个数据集结构
#' @param ds1 用于比较的数据集结构，可使用\code{\link{ds_schema}}获得
#' @param ds2 用于参考的数据集结构，可使用\code{\link{ds_schema}}获得
#' @family dataset function
#' @export
ds_diff_dataset <- function(ds1, ds2) {
  if(rlang::is_empty(ds1)) {
    stop("ds1 is NULL and failed to compare")
  }
  if(rlang::is_empty(ds2)) {
    stop("ds1 is NULL and failed to compare")
  }
  ds_diff_schema(ds_schema(ds1), ds_schema(ds2))
}

#' @title 去除重复行
#' @description
#' 按键值列去除重复行。
#' @param ds 要确认的数据集
#' @param columns 要确认的列名或其向量、列表
#' @family dataset function
#' @export
ds_as_unique <- function(ds, keyColumns) {
  ds[!duplicated(ds[,keyColumns]),]
}

#' @title 定义从任务文件夹导入数据集的函数
#' @description 在定义增量导入任务时需要定义按数据集导入的函数
#' @details 
#' 所定义的导入函数，需要读取数据，就要组装数据所在位置，规则如下：
#' {IMPORT}/{__DOING_TASK_FOLDER__}/{dsName}
#' 其中，__DOING_TASK_FOLDER__就是当前正在处理的任务文件夹名称，
#' 而dsName就是任务文件夹下的数据集文件夹，数据应当放在dsName文件夹内
#' 
#' 举一个例子，如果IMPORT目录位置是："~/glimmer/IMPORT"
#' 要导入的数据位置是："~/glimmer/IMPORT/task1/mycsv/1.csv"
#' 其中，task1就是导入文件夹，mycsv就是数据集。
#' 
#' 考虑增量导入的情况，在新的数据到位后，
#' 可能多了一个这样的文件："~/glimmer/IMPORT/task2/mycsv/1.csv"
#' 此时要做的，就是针对task2进行导入。
#' 而glimmer包提供的import_todo函数就是自动识别出新增的task2，并执行导入脚本。
#' 
#' @param dsName 要导入的数据集名称
#' @param fun 导入函数定义，要求是一个匿名函数
#' @param topic 导入文件夹所在的主题域
#' @family dataset functions
#' @export
ds_import <- function(dsName, fun, topic = "IMPORT") {
  path <- get_path(topic, get_topic("__DOING_TASK_FOLDER__"), dsName)
  if(path |> fs::dir_exists()) {
    fun(path)
  }
}
