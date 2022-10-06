## 创建数据集 -------------

#' @title 初始化数据集配置
#' @description 
#' 
#' 1、通过dsName和topic识别数据集和配置文件位置。
#' 
#' 2、数据集必须首先使用\code{ds_init}函数创建配置文件（或手工建立），
#' 然后才能对数据集做增删改查操作。
#' 
#' 3、所有数据集的存储结构都使用@action作为第一层分区：
#' __APPEND__, __DELETE__, __ARCHIVE__
#' 
#' @param dsName 数据集名称
#' @param data 样本数据，用来分析数据结构
#' @param topic 数据集保存的主题目录，默认为CACHE
#' @param partColumns 支持分区存储，支持多列
#' @param keyColumns 要求在所有行中具有唯一值的列向量，支持多列
#' @param suggestedColumns 查看数据时推荐的显示列向量，支持多列，
#' @param titleColumn 标题列，不支持多列
#' @param desc 对数据集的额外描述
#' @family dataset function
#' @export
ds_init <- function(dsName,
                    topic = "CACHE",
                    data = tibble(),
                    schema = list(),
                    partColumns = c(), keyColumns = c(),
                    suggestedColumns = c(), titleColumn = c(),
                    desc = "-",
                    type = "__UNKNOWN__") {
  ## 要补充的元数据
  meta <- list(
    ## 所有数据集使用@action作为第一层分区：
    ## __NEW__, __UPDATE__, __DELETE__, __ARCHIVE__
    "schema" = schema,
    "partColumns" = c("@action", partColumns) |> unique() |> unlist(),
    "keyColumns" = keyColumns |> unique(),
    "writing" = list(
      ## 当键值重复时，使用新数据替换旧数据
      "updateMode" = "overwrite"),
    "reading" = list(
      "suggestedColumns" = suggestedColumns,
      "titleColumn" = titleColumn))
  
  ## 写入配置文件
  ds_yaml_write(dsName = dsName, meta = meta, data = data, topic = topic, type = type)
}

#' @title 批量追加更新的数据
#' @description 
#' 为了避免修改原数据文件，追加数据时不查询旧数据，提交归档时再统一处理。
#' 
#' 当追加的数据量与归档数据相比规模较小时，这种模式将具有优势。
#' @param d 要写入的数据
#' @param dsName 数据集名称
#' @param topic 数据集保存的主题目录，默认为CACHE
#' @param toDelete 将数据标记为删除
#' @family dataset function
#' @export
ds_append <- function(d, dsName, topic = "CACHE", toDelete = FALSE) {
  if(rlang::is_empty(d)) {
    warning("Empty Dataset to write >>> ", dsName, "/", topic)
  } else {
    if(nrow(d) == 0) {
      warning("No Content in New Dataset to write >>> ", dsName, "/", topic)
    } else {
      ## 默认从CACHE任务目录读写数据集
      path <- get_path(topic, dsName)
      
      ## 读取数据集配置
      meta <- ds_yaml(dsName, topic)
      if(rlang::is_empty(meta)) {
        stop("Empty Dataset Metadata!!!")
      }
      
      ## 追加数据时，确定数据结构一致
      schema_old <- ds_yaml_schema(dsName, topic)
      diff_info <- ds_diff_schema(schema_old, ds_schema(d)) |> filter(!equal)
      if(nrow(diff_info) > 0) {
        print(diff_info |> rename(`F.Old` = `fieldType.x`, `F.New` = `fieldType.y`))
        stop("Different Schema >> ", path)
      }    

      ## 写入数据
      batch <- gen_batchNum()
      d |>
        mutate(cyl = as.integer(cyl)) |>
        mutate(`@deleted` = toDelete) |>
        mutate(`@action` = "__APPEND__") |>
        mutate(`@batch` = batch) |>
        mutate(`@lastmodifiedAt` = lubridate::now(tzone = "Asia/Shanghai")) |>
        ungroup() |>
        arrow::write_dataset(
          path = path,
          format = "parquet",
          basename_template = paste0("append-", batch, "-{i}.parquet"),
          partitioning = meta$partColumns,
          version = "2.0",
          existing_data_behavior = "overwrite")
    }
  }
}

#' @title 批量追加删除的数据
#' @description 以追加标记文件的形式删除数据（不做物理删除）
#' @param d 要删除的数据
#' @param dsName 数据集名称
#' @param topic 数据集保存的主题目录，默认为CACHE
#' @family dataset function
#' @export
ds_delete <- function(d, dsName, topic = "CACHE") {
  meta <- ds_yaml(dsName, topic)
  if(rlang::is_empty(meta$keyColumns)) {
    stop("Can't Delete without keyColumns Setting!!!")
  }
  ds_append(d, dsName = dsName, topic = topic, toDelete = TRUE)
}

#' @title 数据集归档
#' @description 消除零散的追加文件
#' @param dsName 数据集名称
#' @param topic 主题域
#' @details 
#' 归档操作可能耗时，应当集中处理。
#' 如果不归档，则可能因为零散文件较多，而影响数据读取速度
#' @export
ds_submit <- function(dsName, topic = "CACHE") {
  meta <- ds_yaml(dsName, topic)
  path <- get_path(topic, dsName)
  
  ## 找出更新数据的分区
  part <- meta$partColumns[meta$partColumns != "@action"]
  if(rlang::is_empty(part)) {
    d <- ds_read(dsName, topic, noDeleted = FALSE) |> collect()
  } else {
    to_append <-
      arrow::open_dataset(path, format = "parquet") |>
      filter(`@action` ==  "__APPEND__") |>
      collect()
    ## 仅归档__APPEND__数据中包含的分区
    d <- ds_read(dsName, topic, noDeleted = FALSE) |>
      semi_join(to_append, by = part) |>
      collect()
  }

  if(rlang::is_empty(d)) {
    warning("Empty Dataset to submit >>> ", dsName, "/", topic)
  } else {
    if(nrow(d) == 0) {
      warning("No Content in New Dataset to submit >>> ", dsName, "/", topic)
    } else {
      ## 写入数据
      batch <- gen_batchNum()
      d |>
        mutate(cyl = as.integer(cyl)) |>
        mutate(`@action` = "__ARCHIVE__") |>
        ungroup() |>
        arrow::write_dataset(
          path = path,
          format = "parquet",
          basename_template = paste0("archive-", batch, "-{i}.parquet"),
          partitioning = meta$partColumns,
          version = "2.0",
          existing_data_behavior = "delete_matching")
      ## 删除__APPEND__分区
      path_remove <- get_path(topic, dsName, "@action=__APPEND__")
      if(fs::dir_exists(path_remove)) {
        fs::dir_delete(path_remove)
      }
    }
  }
}

#' @title 读取数据集的文件信息
#' @param dsName 数据集名称
#' @param topic 主题域
#' @family dataset function
#' @export
ds_files <- function(dsName, topic = "CACHE") {
  path <- get_path(topic, dsName)
  arrow::open_dataset(path)$files
}

#' @title 读取数据集
#' @param dsName 数据集名称
#' @param topic 主题域
#' @param toFix 修复因归档后未及时清理__APPEND__数据遗漏的部分
#' @param noDeleted 不返回标记为删除的数据
#' @family dataset function
#' @export
ds_read <- function(dsName, topic = "CACHE", toFix = TRUE, noDeleted = TRUE) {
  meta <- ds_yaml(dsName, topic)
  path <- get_path(topic, dsName)
  
  d <- arrow::open_dataset(path, format = "parquet")
  
  if(length(d$files) == 0) {
    return(tibble())
  }

  ## 如果存储结构不包含@action、@lastmodifiedAt等元字段就抛异常
  metaColumns <- c("@action", "@lastmodifiedAt", "@batch", "@deleted")
  if(FALSE %in% (metaColumns %in% names(d))) {
    stop("Dataset storage lost some meata column: ", metaColumns)
  }
  
  ## 如果配置文件中存在主键
  if(length(meta$keyColumns) > 0) {
    ## 提取__APPEND__数据，且不与__ARCHIVE__数据重复（归档后但未删除）
    keys0 <- d |>
      filter(`@action` == "__APPEND__") |>
      select(meta$keyColumns, "@action", "@lastmodifiedAt", "@deleted")
    ## 如果__ARCHIVE__数据保存后，未及时删除__APPEND__，读取时将丢失这部分数据
    ## 这需要增加以下的判断步骤，但需要额外付出计算性能
    if(toFix) {
      keys0 <- keys0 |>
        anti_join(d |>
                  select(meta$keyColumns, "@action", "@lastmodifiedAt") |>
                  filter(`@action` == c("__ARCHIVE__")),
                by = c(meta$keyColumns, "@lastmodifiedAt"))}
    ## 提取应保留的最近一次__APPEND__数据
    keys <- keys0 |>
      select(meta$keyColumns, "@action", "@lastmodifiedAt", "@deleted") |>
      semi_join(keys0 |>
                  group_by(!!!syms(meta$keyColumns)) |>
                  summarise(`@lastmodifiedAt` = max(`@lastmodifiedAt`), .groups = "drop") |>
                  collect(), by = c(meta$keyColumns, "@lastmodifiedAt"))
  } else {
    keys <- tibble()
  }

  if(!rlang::is_empty(keys)) {
    ## 旧数据要更新的记录
    arrchive_expired <- d |>
      filter(`@action` == "__ARCHIVE__") |>
      semi_join(keys |> filter(`@action` == "__APPEND__"), by = meta$keyColumns)
    ## 追加数据中过期的记录
    append_expired <- d |>
      filter(`@action` == "__APPEND__") |>
      anti_join(keys, by = c(meta$keyColumns, "@lastmodifiedAt"))
    ## 剔除：过期旧数据和过期新数据, 以及标记为删除的
    resp <- d |>
      anti_join(arrchive_expired, by = c(meta$keyColumns, "@lastmodifiedAt")) |>
      anti_join(append_expired, by = c(meta$keyColumns, "@lastmodifiedAt"))
  } else {
    ## 数据没有追加操作
    resp <- d
  }
  
  ## 处理标记为删除的数据
  if(noDeleted) {
    resp <- resp |> filter(!`@deleted`)
  }

  ## 按元数据中的读取建议整理结果
  if(!rlang::is_empty(meta$suggestedColumns)) {
    resp |> select(!!!syms(meta$suggestedColumns), everything())
  } else {
    resp
  }
}
## 列举

#' @title 写入ApachheParquet文件集
#' @param d 要写入的数据
#' @param dsName 数据集名称
#' @param topic 数据集保存的主题目录，默认为CACHE
#' @param partColumns 照此进行分区存储，支持多列
#' @param keyColumns 要求在所有行中具有唯一值的列向量，支持多列
#' @param suggestedColumns 查看数据时推荐的显示列向量，支持多列，
#' @param titleColumn 标题列，不支持多列
#' @param desc 对数据集的额外描述
#' @param mode 当主键重复时，update模式（默认）使用新数据，append模式保留旧数据
#' @description 更新受影响的分区
#' @family dataset function
#' @details
#' 默认为更新模式；如果仅直接覆写数据，应先删除数据集
#' 
#' 如果不设置关键列，则为追加模式；否则按关键列替换
#' 
#' @export
ds_write <- function(d, dsName, topic = "CACHE",
                     partColumns = c(), keyColumns = c(),
                     suggestedColumns = c(), titleColumn = c(),
                     desc = "-", mode = "update") {
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
    
    ## 找出受影响的分区文件（如果没有分区就提取整个数据集）
    ## 注意：要找出的是分区文件，而不是数据
    if(rlang::is_empty(partColumns)) {
      affected_data <- arrow::open_dataset(path, format = "parquet") |> collect()
    } else {
      affected_data <- arrow::open_dataset(path, format = "parquet") |>
        semi_join(d, by = partColumns) |> collect()
    }
  }
  
  ## 当写入数据到磁盘时
  if(nrow(d) > 0) {
    ## 没有设置关键列，直接追加到旧数据分区文件中
    if(rlang::is_empty(keyColumns)) {
      to_write <- affected_data |> rbind(d)
    } else {
      if(rlang::is_empty(affected_data)) {
        to_write <- d
      } else {
        if(mode == "append") {
          ## 追加模式，按关键列剔除新数据
          to_write <- affected_data |> rbind(d |> anti_join(affected_data, by = keyColumns))
        } else {
          ## 更新模式，按关键列剔除旧数据
          to_write <- (affected_data |> anti_join(d, by = keyColumns)) |> rbind(d)
        }
      }
    }

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
    list(
      "desc" = desc,
      "nrow" = nrow(d),
      "columns" = names(d),
      "updated" = list(
        "lastUpdatedAt" = lubridate::as_datetime(updateTimestamp, tz = "Asia/Shanghai") |> as.character(),
        "lastAffectedSummary" = updated,
        "lastAffectedFiles" = affectedParts$path)
    ) |> ds_write_yaml(dsName, topic)
  }
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
        x <- yaml::read_yaml(path)
        list(
          "datasetId" = x$datasetId,
          "topic" = x$topic,
          "type" = x$type,
          "name" = x$name,
          "desc" = x$desc
        )
      })
  } else {
    tibble()
  }
}

#' @title 移除数据文件夹
#' @description 可以整个移除，也可以组装目录后按分区移除
#' @param dsName 数据集名称
#' @param topic 主题域
#' @family dataset function
#' @export
ds_remove_path <- function(dsName, topic = "CACHE") {
  path <- get_path(topic, dsName) |> fs::path_expand()
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

