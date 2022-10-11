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
import_init <- function(dsName = "__IMPORT_FILES__", cacheTopic = "CACHE") {
  ## 任务数据样本
  sampleData <- tibble(
    "importTopic" = dt_string(),
    "createdAt" = dt_datetime(), # birth_time,
    "scanedAt" = dt_datetime(),
    "lastmodifiedAt" = dt_datetime(), # modification_time
    "batchFolder" = dt_string(),
    "filePath" = "MY/IMPORT/FOLDER/my.csv",
    "fileSize" = 537.0, # fs::bytes
    "taskTopic" = "TASK", # 匹配到的任务主题，"-" default
    "taskId" = dt_string(),  # 匹配到的任务，"-" default
    "queueBatchId" = dt_string(), # 队列中的@batchId
    "taskReadAt" = dt_datetime(), # 已被任务读取
    "taskRunAt" = dt_datetime(), # 任务开始运行
    "taskDoneAt" = dt_datetime(), # 任务完成
    "ignore" = dt_bool(), # 忽略处理后，不必再判断处理时间
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

#' @title 列举所有导入素材文件
#' @family import function
#' @export
import_files_all <- function(importTopic = "IMPORT") {
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
        batchPath |>
          fs::dir_info(type = "file", recurse = T) |>
          as_tibble() |>
          rename(lastmodifiedAt=modification_time, createdAt=birth_time, fileSize=size) |>
          mutate(filePath = stringr::str_remove(path, paste0(batchPath, "/"))) |>
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
import_files_scan <- function(importDataset = "__IMPORT_FILES__", importTopic = "IMPORT", cacheTopic = "CACHE") {
  all_files <- import_files_all(importTopic)
  if(!rlang::is_empty(all_files)) {
    ## 筛查未曾入库或虽曾入库但已修改的素材文件
    existing <- ds_read(dsName = importDataset, topic = cacheTopic) |>
      collect()
    if(rlang::is_empty(existing)) {
      newScan <- all_files
    } else {
      newScan <- all_files |>
        mutate(eTag = as.integer(lastmodifiedAt)) |>
        anti_join(existing |> mutate(eTag = as.integer(lastmodifiedAt)),
                  by = c("filePath", "eTag"))
    }
    ## 入库
    if(nrow(newScan) > 0) {
      newScan |> ds_append(dsName = importDataset, topic = cacheTopic)
      ds_submit(dsName = importDataset, topic = cacheTopic)
    } else {
      message("No new files to import!!")
    }
  }
}

#' @title 扫描所有未处理、未忽略的文件
#' @family import function
#' @export
import_dataset_read <- function(importDataset = "__IMPORT_FILES__",
                              cacheTopic = "CACHE",
                              ignoreFlag = FALSE,
                              newFilesFlag = TRUE) {
  filesToRead <- ds_read(dsName = importDataset, topic = cacheTopic)
  if(!rlang::is_empty(filesToRead)) {
    d0 <- filesToRead |> filter(is.na(taskReadAt) == newFilesFlag)
    d0 |> filter(ignore == ignoreFlag)
  } else {
    tibble()
  }
}

#' @title 为导入素材匹配任务
#' @description 
#' 开发过程中，可能要为新建任务重新匹配导入素材。
#' 
#' 此时，可以根据实际情况决定仅针对未处理状态还是所有的导入素材做匹配。
#' @family import function
#' @export
import_dataset_task_match <- function(importDataset = "__IMPORT_FILES__",
                              taskTopic = "TASK_DEFINE",
                              cacheTopic = "CACHE",
                              onlyNewFiles = TRUE) {
  ## 扫描所有未处理、未忽略的文件
  filesToRead <- import_dataset_read(importDataset = importDataset, cacheTopic = cacheTopic) |>
    filter(is.na(taskId)) |> collect()
  ## 匹配任务
  if(!rlang::is_empty(filesToRead)) {
    ## 按照约定，使用taskId匹配导入素材的filePath
    allTasks <- task_all(taskTopic)
    hasMatched <- FALSE
    if(!rlang::is_empty(allTasks)) {
      allTasks |>
        filter(taskType == "__IMPORT__" & online) |>
        select(taskTopic, taskId) |>
        purrr::pwalk(function(taskTopic, taskId) {
          item <- list("taskTopic" = taskTopic, "taskId" = taskId)
          pat <- paste0("^", taskId)
          matched <- filesToRead |>
            filter(stringr::str_detect(filePath, pat)) |>
            mutate(taskTopic = item$taskTopic, taskId = item$taskId)
          if(nrow(matched) > 0) {
            matched |> ds_append(dsName = importDataset, topic = cacheTopic)
            hasMatched <<- TRUE
          }
        })
      if(hasMatched) {
        ds_submit(dsName = importDataset, topic = cacheTopic)
      } else {
        message("No import files to match with task defined!!")
      }
    }
  }
}

#' @title 构建导入任务的批处理队列
#' @description 
#' 扫描素材时，要求遵循文件夹命名约定：即被扫描文件相对于批次文件夹的相对路径名，
#' 以扫描任务TaskId开始，例如\code{联系人/重要/1.csv}可以匹配TaskID为\code{联系人}的任务，
#' 也可以同时匹配到\code{联系人/重要}的任务。
#' 匹配到的任务都会将素材文件作为参数执行。
#' 
#' 扫描后的任务处理流程如下：
#' 
#' （1）将未处理的重复素材设置忽略标记；
#' 
#' （2）罗列所有扫描任务；
#' 
#' （3）根据扫描任务优先级，查找未处理素材，添加导入任务；
#' 
#' （4）导入素材添加后，更新这些素材文件的状态信息（读取时间）。
#' 
#' （5）每次处理的素材文件个数不超过100个，超过时建立新任务；
#' 
#' （6）追加一个任务：导入结束后，更新这些素材文件的状态信息。
#' @family import function
#' @export
import_task_queue_create <- function(taskQueue = "__TASK_QUEUE__", importDataset = "__IMPORT_FILES__",
                            taskTopic = "TASK_DEFINE", importTopic = "IMPORT", cacheTopic = "CACHE") {
  ## 扫描所有未处理、未忽略、已完成匹配的导入素材文件
  filesToRead <- ds_read(dsName = importDataset, topic = cacheTopic) |>
    filter(is.na(taskReadAt)) |>
    filter(!ignore) |>
    filter(!is.na(taskId) & !is.na(taskTopic)) |>
    collect()
  if(nrow(filesToRead) > 0) {
    tasksToCreate <- filesToRead |>
      group_by(taskTopic, taskId) |>
      nest() |>
      purrr::pmap_df(function(taskTopic, taskId, data) {
        params <- list(
          input = list(
            "batchFolder" = data$batchFolder,
            "filePath" = data$filePath,
            "importTopic" = importTopic,
            "path" = get_path(importTopic, data$batchFolder, data$filePath),
            "cacheTopic" = cacheTopic,
            "taskId" = taskId,
            "taskTopic" = taskTopic)) |>
          task_queue_param_to_yaml()
        task_queue_item(taskId = taskId,
                        params = params,
                        taskType = "__IMPORT__",
                        taskTopic = taskTopic)
      })
    ## 增加导入任务到队列
    batchId <- tasksToCreate |> ds_append(dsName = taskQueue, topic = cacheTopic)      
    ## 修改导入素材文件状态
    filesToRead |>
      mutate(taskReadAt = now(tzone = "Asia/Shanghai"), queueBatchId = batchId) |>
      ds_append(dsName = importDataset, topic = cacheTopic)
  } else {
    message("No tasks into queue!!")
  }
}

#' @title 执行导入任务
#' @family import function
#' @export
import_task_queue_run <- function(taskQueue = "__TASK_QUEUE__", cacheTopic = "CACHE",
                                  importDataset = "__IMPORT_FILES__") {
  q <- task_queue_todo(taskTypes = "__IMPORT__", dsName = taskQueue, cacheTopic = cacheTopic)
  if(nrow(q) > 0) {
    q |> group_by(`@batchId`) |>
      nest() |>
      rename(batchId = `@batchId`) |>
      purrr::pwalk(function(batchId, data) {
        #
        import_dataset_read(importDataset, cacheTopic, newFilesFlag = FALSE) |>
          filter(queueBatchId %in% batchId) |>
          collect() |>
          mutate(taskRunAt = now(tzone = "Asia/Shanghai")) |>
          ds_append(importDataset, cacheTopic)
        #
        task_queue_run(data)
        #
        import_dataset_read(importDataset, cacheTopic, newFilesFlag = FALSE) |>
          filter(queueBatchId %in% batchId) |>
          collect() |>
          mutate(taskDoneAt = now(tzone = "Asia/Shanghai")) |>
          ds_append(importDataset, cacheTopic)
      })
    ds_submit(importDataset, cacheTopic)
  } else {
    message("No tasks to run !!")
  }
}
