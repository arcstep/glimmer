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
    "desc" = desc,
    "schema" = schema,
    "partColumns" = c("@action", partColumns) |> unique() |> unlist(),
    "keyColumns" = keyColumns |> unique(),
    "suggestedColumns" = suggestedColumns,
    "titleColumn" = titleColumn)
  
  ## 写入配置文件
  ds_yaml_write(dsName = dsName, meta = meta, data = data, topic = topic, type = type)
}

#' @title 规范数据存储类型
#' @family dataset function
#' @export
ds_type <- function(type = "__UNKNOWN__") {
  if(type %in% c("__STATE__", "__IMPORT__", "__BUILD__", "__RISK__")) {
    type
  } else {
    "__UNKNOWN__"
  }}

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
ds_append <- function(d, dsName, topic = "CACHE") {
  if(is_empty(d)) {
    warning("Empty Dataset to write >>> ", dsName, "/", topic)
  } else {
    if(nrow(d) == 0) {
      warning("No Content in New Dataset to write >>> ", dsName, "/", topic)
    } else {
      ## 默认从CACHE任务目录读写数据集
      path <- get_path(topic, dsName)
      
      ## 读取数据集配置
      meta <- ds_yaml(dsName, topic)
      if(is_empty(meta)) {
        stop("Empty Dataset Metadata!!!")
      }
      
      ## 缺少schema定义
      if(is_empty(meta$schema)) {
        stop("No Schema Defined!!!")
      }

      ## 缺少主键字段
      if(!is_empty(meta$keyColumns)) {
        if(FALSE %in% (meta$keyColumns %in% names(d))) {
          stop("No keyColumns in data: ", meta$keyColumns |> paste(collapse = ", "))
        }
      }
      
      ## 缺少分区字段
      if(!is_empty(meta$partColumns)) {
        if(FALSE %in% (meta$partColumns %in% c("@action", names(d)))) {
          stop("No partColumns in data: ", meta$partColumns |> paste(collapse = ", "))
        }
      }
      
      ## 写入数据
      batch <- gen_batchNum()
      x <- d |>
        mutate(`@action` = "__APPEND__") |>
        mutate(`@batchId` = batch) |>
        mutate(`@lastmodifiedAt` = lubridate::now(tzone = "Asia/Shanghai"))
      ## 如果没有提供删除标记，就补充这一列
      if("@deleted" %nin% names(d)) x[["@deleted"]] <- FALSE
      ## 写入parquet文件组
      x |>
        ungroup() |>
        write_dataset(
          path = path,
          format = "parquet",
          write_statistics = TRUE,
          basename_template = paste0("append-", batch, "-{i}.parquet"),
          partitioning = meta$partColumns,
          version = "2.0",
          existing_data_behavior = "overwrite")
        message("data append <", dsName, ">", " ", nrow(x), " rows wroted!")
      return(batch)
    }
  }
  return(NULL)
}

#' @title 写入数据
#' @description 先执行追加操作，然后直接归档整理。
#' @param d 要追加的数据
#' @param dsName 数据集名称
#' @param topic 数据集保存的主题目录，默认为CACHE
#' @family dataset function
#' @export
ds_write <- function(d, dsName, topic = "CACHE") {
  ds_append(d, dsName, topic)
  ds_submit(dsName, topic)
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
  if(is_empty(meta$keyColumns)) {
    stop("Can't Delete without keyColumns Setting!!!")
  }
  ds_read(dsName, topic) |>
    semi_join(d, by = meta$keyColumns) |>
    collect() |>
    ds_as_deleted() |>
    ds_append(dsName = dsName, topic = topic)
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
  
  ## 提前列出准备删除后的分区文件
  allFiles <- ds_files(dsName, topic)
  toRemoveFiles <- allFiles[stringr::str_detect(allFiles, "@action=__APPEND__")]
  force(toRemoveFiles)
  ## 找出更新数据的分区
  # part <- meta$partColumns[meta$partColumns != "@action"]
  # if(is_empty(part)) {
    d <- ds_read(dsName, topic, noDeleted = FALSE) |> collect()
  # } else {
  #   myschema <- do.call("schema", ds_schema_obj(dsName, topic))
  #   to_append <- open_dataset(path, format = "parquet", schema = myschema) |>
  #     filter(`@action` ==  "__APPEND__") |>
  #     collect()
  #   ## 仅归档__APPEND__数据中包含的分区
  #   ## semi_join执行效率较低，暂时使用全量覆盖
  #   d <- ds_read(dsName, topic, noDeleted = FALSE) |>
  #     semi_join(to_append, by = part) |>
  #     collect()
  # }

  if(is_empty(d)) {
    warning("Empty Dataset to submit >>> ", dsName, "/", topic)
  } else {
    if(nrow(d) == 0) {
      warning("No Content in New Dataset to submit >>> ", dsName, "/", topic)
    } else {
      ## 写入数据
      batch <- gen_batchNum()
      d |>
        mutate(`@action` = "__ARCHIVE__") |>
        ungroup() |>
        write_dataset(
          path = path,
          format = "parquet",
          write_statistics = TRUE,
          basename_template = paste0("archive-", batch, "-{i}.parquet"),
          partitioning = meta$partColumns,
          version = "2.0",
          existing_data_behavior = "delete_matching")
      ## 删除已经处理过的文件和文件夹
      ## 如果在写入过程中新增文件，则保留
      allFiles2 <- ds_files(dsName, topic)
      allFiles3 <- allFiles2[stringr::str_detect(allFiles2, "@action=__APPEND__")]
      newFiles <- allFiles3[allFiles3 %nin% toRemoveFiles]
      toRemoveFolders <- fs::path_dir(toRemoveFiles) |> unique()
      toReservedForlders <- fs::path_dir(newFiles) |> unique()
      ## 先删除文件夹
      toRemoveFolders[toRemoveFolders %nin% toReservedForlders] |> fs::dir_delete()
      ## 再删除文件
      toRemoveFiles |> purrr::walk(function(p) {
        if(fs::file_exists(p)) fs::file_delete(p)
      })
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
  open_dataset(path)$files
}

#' @title 仅读取已归档数据
#' @description 
#' 仅返回已归档数据
#' @param dsName 数据集名称
#' @param topic 主题域
#' @param noDeleted 不返回标记为删除的数据
#' @family dataset function
#' @export
ds_read0 <- function(dsName, topic = "CACHE", noDeleted = TRUE) {
  meta <- ds_yaml(dsName, topic)
  path <- get_path(topic, dsName)
  
  ## 按照yaml配置中的schema读取数据集
  myschema <- do.call("schema", ds_schema_obj(dsName, topic))
  d <- open_dataset(path, format = "parquet", schema = myschema)
  
  ##
  if(length(d$files) == 0) return(tibble())
  
  ##
  d <- d |> filter(`@action` == "__ARCHIVE__" & `@deleted` == !noDeleted)
  ## 按元数据中的读取建议整理结果
  if(!is_empty(meta$suggestedColumns)) {
    d |> select(!!!syms(meta$suggestedColumns), everything())
  } else {
    d
  }
}

#' @title 判断数据集是否已经定义
#' @param dsName 数据集名称
#' @param topic 主题域
#' @family dataset function
#' @export
ds_exists <- function(dsName, topic = "CACHE") {
  !is_empty(ds_yaml(dsName))
}

#' @title 读取数据集
#' @param dsName 数据集名称
#' @param topic 主题域
#' @param noDeleted 不返回标记为删除的数据
#' @family dataset function
#' @export
ds_read <- function(dsName, topic = "CACHE", noDeleted = TRUE) {
  meta <- ds_yaml(dsName, topic)
  path <- get_path(topic, dsName)

  ## 按照yaml配置中的schema读取数据集
  myschema <- do.call("schema", ds_schema_obj(dsName, topic))
  d <- open_dataset(path, format = "parquet", schema = myschema)
  
  if(length(d$files) == 0) {
    return(tibble())
  }

  ## 如果存储结构不包含@action、@lastmodifiedAt等元字段就抛异常
  metaColumns <- c("@action", "@lastmodifiedAt", "@batchId", "@deleted")
  if(FALSE %in% (metaColumns %in% names(d))) {
    stop("Dataset storage lost some meata column: ", metaColumns |> paste(collapse = ", "))
  }
  
  ## 如果配置文件中存在主键
  if(length(meta$keyColumns) > 0) {
    ## 提取__APPEND__数据，且不与__ARCHIVE__数据重复（归档后但未删除）
    keys0 <- d |>
      filter(`@action` == "__APPEND__") |>
      select(meta$keyColumns, "@action", "@lastmodifiedAt", "@deleted")
    ## 如果__ARCHIVE__数据保存后，未及时删除__APPEND__，读取时将丢失这部分数据
    ## 这可能是因为ds_submit执行过程中可能因为ds_read或ds_write执行时间过长
    ##
    ## 这可以在ds_read中剔除，这需要增加以下的判断步骤，但需要额外付出计算性能
    ## 这需要增加以下的判断步骤，但需要额外付出计算性能
    keys0 <- keys0 |>
      anti_join(d |>
                select(meta$keyColumns, "@action", "@lastmodifiedAt") |>
                filter(`@action` == c("__ARCHIVE__")),
              by = c(meta$keyColumns, "@lastmodifiedAt"))
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

  if(!is_empty(keys)) {
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
  if(!is_empty(meta$suggestedColumns)) {
    resp |> select(!!!syms(meta$suggestedColumns), everything())
  } else {
    resp
  }
}

#' @title 列举所有数据集
#' @param topic 主题域
#' @family dataset function
#' @export
ds_all <- function(topic = "CACHE") {
  root_path <- get_path(topic)
  if(fs::dir_exists(root_path)) {
    fs::dir_ls(root_path, type = "file", all = T, glob = "*.yml", recurse = T) |>
      purrr::map_df(function(path) {
        x <- yaml::read_yaml(path)
        list(
          "datasetId" = x$datasetId,
          "type" = x$type,
          "name" = x$name,
          "desc" = x$desc
        )
      })
  } else {
    tibble()
  }
}

#' @title 移除数据集
#' @description 可以整个移除，也可以组装目录后按分区移除
#' @param dsName 数据集名称
#' @param topic 主题域
#' @family dataset function
#' @export
ds_drop <- function(dsName, topic = "CACHE") {
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
#' 查看数据集架构时，必须包含数据。
#' 
#' @param ds 数据框
#' @family dataset function
#' @export
ds_schema <- function(ds) {
  if(is_empty(ds)) {
    tibble()
  } else {
    # tibble(
    #   fieldName = names(ds),
    #   fieldType = lapply(names(ds), function(field) {typeof(ds[[field]])}) |> unlist()
    # ) |> arrange(.data[["fieldName"]])
    ## 写入到临时文件，再提取类型信息
    path <- tempfile()
    ds |>
      head() |>
      write_dataset(path = path, format = "parquet", write_statistics = FALSE, version = "2.0")
    s <- (open_dataset(path, format = "parquet"))$schema
    myschema <- s$fields |>
      purrr::map_df(function(item) {
        str <- item$ToString() |> stringr::str_split(": ")
        list("fieldName" = str[[1]][[1]], "fieldType" = str[[1]][[2]])
      })
    path |> fs::file_delete()
    myschema
  }
}

#' @title 构造Arrow的schema
#' @description 
#' 从数据框格式的schema转换为Arrow的Schema对象
#' 
#' @param dsName 数据集名称
#' @param topic 数据集保存的主题目录，默认为CACHE
#' @family dataset function
#' @export
ds_schema_obj <- function(dsName, topic = "CACHE") {
  s <- list()
  yml <- ds_yaml(dsName, topic)
  if(!is_empty(yml$schema)) {
    yml$schema |> purrr::walk(function(item) {
      s[[item$fieldName]] <<- dt_field(item$fieldType)
    }) 
    s$`@from` <- dt_field("string")
    s$`@deleted` <- dt_field("bool")
    s$`@action` <- dt_field("string")
    s$`@batchId` <- dt_field("string")
    s$`@lastmodifiedAt` <- dt_field("timestamp[us, tz=Asia/Shanghai]")
  } else {
    stop("No Schema in metada: ", topic, "/", dsName, "/.metadata.yml")
  }
  s
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
  if(is_empty(ds1)) {
    stop("ds1 is NULL and failed to compare")
  }
  if(is_empty(ds2)) {
    stop("ds1 is NULL and failed to compare")
  }
  ds_diff_schema(ds_schema(ds1), ds_schema(ds2))
}

#' @title 去除重复行
#' @description
#' 按键值列去除重复行，保留最先发现的行
#' @param ds 要确认的数据集
#' @param columns 要确认的列名或其向量、列表
#' @family dataset function
#' @export
ds_as_unique <- function(ds, keyColumns) {
  ds[!duplicated(ds[,keyColumns]),]
}

#' @title 转换字符串为时间日期类型
#' @family dataset function
#' @export
ds_as_datetime <- function(ds, timestampColumn, datetimeColumn, tzone = "Asia/Shanghai")
{
  if (is.character(ds[[timestampColumn]])) {
    mutate(ds, `:=`(
      {{datetimeColumn}},
      as_datetime(as.integer(stringi::stri_sub(!!sym(timestampColumn), to = 10)), tz = tzone)))
  }
  else if (is.numeric(ds[[timestampColumn]])) {
    mutate(ds, `:=`(
      {{datetimeColumn}},
      as_datetime(as.integer(!!sym(timestampColumn)), tz = tzone)))
  }
  else {
    ds
  }
}

#' @title 设置数据来源
#' @family dataset function
#' @export
ds_as_from <-function(ds, sourceFrom) {
  ds["@from"] <- sourceFrom
  ds
}

#' @title 设置删除标记
#' @family dataset function
#' @export
ds_as_deleted <-function(ds, flag = TRUE) {
  ds["@deleted"] <- flag
  ds
}  
  