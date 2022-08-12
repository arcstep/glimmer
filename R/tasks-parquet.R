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
write_dataset <- function(d, dsName, topic = "CACHE", partColumns = c(), keyColumns = c(), desc = "-") {
  ## 默认从CACHE任务目录读写数据集
  path <- get_path(topic, dsName)
  
  ## 确定数据集不为空
  if(rlang::is_empty(d)) stop("Empty Dataset to write >>> ", path)
  if(!is.data.frame(d)) stop("Not Tibble Object to write >>> ", path)
  if(nrow(d)==0) warning("No Content in New Dataset to write >>> ", path)
  
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
    beginTimestamp <- lubridate::now()
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
    message("write_dataset << ", dsName, " >>", updated)
    write_state(
      stateName = "__WRITE_DATASET__",
      tibble(
        "dataset" = dsName,
        "updated" = updated,
        "affected" = affected
      ))
    
    ## 更新元数据集元件
    d <- arrow::open_dataset(path, format = "parquet")
    updateTimestamp <- lubridate::now()
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
#' @family dataset function
#' @export
read_affected_parts <- function(affectedParts) {
  affectedParts |>
    stringr::str_split(",") |>
    unlist() |>
    arrow::open_dataset(format = "parquet")
}

#' @title 读取最近一次更新时重写过分区的数据集文件
#' @family dataset function
#' @export
last_affected_parts <- function(dataset = c()) {
  if(rlang::is_empty(dataset)) {
    state <- read_state("__WRITE_DATASET__") |>
      arrange(desc(lastModified)) |> collect()
  } else {
    state <- read_state("__WRITE_DATASET__") |>
      filter(.data$dataset == dataset) |>
      arrange(desc(lastModified)) |> collect()
  }
  state$affected[[1]] |> read_affected_parts()
}

#' @title 读取数据集
#' @family dataset function
#' @export
read_dataset <- function(dsName, topic = "CACHE") {
  path <- get_path(topic, dsName)
  arrow::open_dataset(path, format = "parquet")
}

#' @title 列举所有数据集
#' @family dataset function
#' @export
all_dataset <- function(topic = "CACHE") {
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

#' @title 移除数据文件夹
#' @description 可以整个移除，也可以组装目录后按分区移除
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
