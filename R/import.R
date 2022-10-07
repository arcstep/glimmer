#' @title 初始化导入状态数据集
#' @description
#' 导入素材中所有文件都将入库。
#' 
#' 每次写入的导入素材文件，应当放置在同一个批次文件夹中。
#' 该批次文件夹应当包含时间要素，以方便按写入时间排序。
#' 
#' 在同一次扫描中，如果不同批次文件夹中的有相同路径的导入素材文件，只关注最新文件
#' 如果有补充采集，则视情况将旧文件标记为“忽略”。
#' @family import function
#' @export
import_dataset_init <- function(dsName = "__IMPORT_FILES__", cacheTopic = "CACHE") {
  ## 任务数据样本
  sampleData <- tibble(
    "importTopic" = "IMPORT",
    "createdAt" = as_datetime("2022-10-01 08:19:45", tz = "Asia/Shanghai"), # birth_time,
    "scanedAt" = as_datetime("2022-10-01 08:19:45", tz = "Asia/Shanghai"),
    "lastmodifiedAt" = as_datetime("2022-10-01 01:19:45", tz = "Asia/Shanghai"), # modification_time
    "batchFolder" = "schedule_2022-10-01",
    "filePath" = "MY/IMPORT/FOLDER/my.csv",
    "fileSize" = 537.0, # fs::bytes
    "taskTopic" = "TASK", # 匹配到的任务主题，"-" default
    "taskId" = "MY_UNIQUE_TASK_NAME",  # 匹配到的任务，"-" default
    "taskReadAt" = as_datetime("2022-10-01 08:19:45", tz = "Asia/Shanghai"), # 已被任务读取
    "taskRunAt" = as_datetime("2022-10-01 08:29:45", tz = "Asia/Shanghai"), # 任务开始运行
    "taskDoneAt" = as_datetime("2022-10-01 08:30:45", tz = "Asia/Shanghai"), # 任务完成
    "ignore" = FALSE, # 忽略处理后，不必再判断处理时间
    "year" = 2022L, # createdAt year
    "month" = 10L)  # createdAt month
  ds_init(
    dsName = dsName,
    topic = cacheTopic,
    data = sampleData,
    keyColumns = c("batchFolder", "filePath"),
    partColumns = c("year", "month"),
    type = "__STATE__")
}

#' @title 列举所有文件
#' @family import function
#' @export
import_folders_all <- function(importTopic = "IMPORT") {
  ## 扫描素材的批次文件夹
  batchFolders <- get_path(importTopic) |>
    fs::dir_ls(type = "dir", recurse = F) |>
    fs::path_abs()
  if(!rlang::is_empty(batchFolders)) {
    ## 扫描任务脚本文件夹
    batchFolders |> sort() |>
      purrr::map(function(batchPath) {
        ## 提取批次文件夹名称
        batchFolderName <- stringr::str_remove(batchPath, paste0(get_path(importTopic), "/"))
        ## 提取所有素材文件
        files <- batchPath |>
          fs::dir_info(type = "file", recurse = T) |>
          as_tibble() |>
          rename(filePath=path, lastmodifiedAt=modification_time, createdAt=birth_time, fileSize=size) |>
          mutate(filePath = stringr::str_remove(filePath, paste0(batchPath, "/"))) |>
          mutate(batchFolder = batchFolderName) |>
          mutate(importTopic = importTopic) |>
          mutate(scanedAt = now(tzone = "Asia/Shanghai")) |>
          select(importTopic, createdAt, scanedAt, lastmodifiedAt, batchFolder, filePath, fileSize) |>
          mutate(year = as.integer(lubridate::year(createdAt))) |>
          mutate(month = as.integer(lubridate::month(createdAt)))
      }) |> purrr::reduce(rbind)
  } else {
    warning("No IMPORT Folder prepared!!!")
    tibble()
  }
}

#' @title 从导入素材中提取未扫描文件
#' @description 
#' 读取导入素材，同时读取导入状态库，
#' 将未曾导入或有修改时间有变动的文件写入状态库。
#' 
#' 处理导入任务时也将修改这一状态库，标记taskReadAt、taskDoneAt、taskDoneAt、ignore等字段。
#' @family import function
#' @export
import_folders_scan <- function(importDataset = "__IMPORT_FILES__", importTopic = "IMPORT", cacheTopic = "CACHE") {
  all_files <- import_folders_all(importTopic)
  if(!rlang::is_empty(all_files)) {
    ## 筛查未曾入库或虽曾入库但已修改的素材文件
    existing <- ds_read(dsName = importDataset, topic = cacheTopic) |> collect()
    if(rlang::is_empty(existing)) {
      newScan <- all_files
    } else {
      newScan <- all_files |>
        anti_join(existing, by = c("filePath", "lastmodifiedAt"))
    }
    ## 入库
    newScan |> ds_append(dsName = importDataset, topic = cacheTopic)
    # ds_submit(dsName = importDataset, topic = cacheTopic)
  }
}

#' @title 将导入素材标记为忽略状态
#' @family import function
#' @export
import_folders_ignore <- function() {}

#' @title 生成导入任务
#' @family import function
#' @export
import_task_gen <- function(taskDataset = "__TASK_QUEUE__", importDataset = "__IMPORT_FILES__",
                            taskTopic = "TASK_DEFINE", importTopic = "IMPORT", cacheTopic = "CACHE") {
  ## 扫描所有未处理文件夹
  filesToRead <- ds_read(dsName = importDataset, topic = cacheTopic) |>
    filter(is.na(taskReadAt)) |>
    collect()
  if(!rlang::is_empty(filesToRead)) {
    ## 从任务清单中找出匹配的导入任务
    ## 按照约定，使用taskId匹配导入素材的filePath
    allTasks <- task_all(taskTopic)
    if(!rlang::is_empty(allTasks)) {
      tasksToGen <- allTasks |>
        filter(taskType == "__TYPE_IMPORT__" & online) |>
        select(taskId) |>
        purrr::pmap_df(function(taskId) {
          pat <- paste0("^", taskId)
          ds <- filesToRead |> filter(stringr::str_detect(filePath, pat))
          if(!rlang::is_empty(ds)) {
            params <- list("filePath" = ds$filePath) |> queue_param_to_yaml()
          } else {
            params <- "NULL"
          }
          createdAt <- now(tzone = "Asia/Shanghai")
          list(
            "id" = gen_batchNum(),
            "taskTopic" = taskTopic,
            "taskType" = "__TYPE_IMPORT__",
            "taskId" = taskId,
            "ymlParams" = params,
            "createdAt" = createdAt,
            "year" = as.integer(lubridate::year(createdAt)),
            "month" = as.integer(lubridate::month(createdAt)))
        }) |> filter(ymlParams != "NULL")
      ## 修改导入素材文件状态
      filesToRead |>
        mutate(taskReadAt = now()) |>
        ds_append(dsName = importDataset, topic = cacheTopic)
      ## 增加导入任务到队列
      tasksToGen |>
        ds_append(dsName = taskDataset, topic = cacheTopic)      
    }
  }
}

#' @title 执行导入任务
#' @family import function
#' @export
import_folders_update <- function() {}
