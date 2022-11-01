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
import_init <- function(dsName = "__IMPORT_FILES__", cacheTopic = "CACHE", snapTopic = "SNAP") {
  ## 任务数据样本
  sampleData <- tibble(
    "importTopic" = dt_string(),
    "createdAt" = dt_datetime(), # birth_time,
    "scanedAt" = dt_datetime(),
    "batchFolder" = "schedual_001",
    "filePath" = "AAA/my.csv",
    "fileSize" = 537.0, # fs::bytes
    "lastmodifiedAt" = dt_datetime(), # modification_time
    "todo" = dt_bool(), # 待处理标志
    "doneAt" = dt_datetime(),  # 任务处理完成时间
    "ignore" = dt_bool(), # 不做处理
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

#' @title 检测发生变化的素材文件
#' @family import function
#' @export
import_changed <- function(importTopic = "IMPORT", snapTopic = "SNAP") {
  if(!fs::file_exists(get_path(snapTopic, "import.rds"))) {
    ## 如果还没有建立快照，就扫描所有批次文件夹
    batchFolders <- fs::dir_ls(get_path(importTopic), type = "dir", recurse = F)
  } else {
    ## 扫描发生变化的素材批次文件夹
    folders <- readRDS(get_path(snapTopic, "import.rds")) |>
      changedFiles()
    batchFolders <- c(
      folders$added[!fs::is_dir(folders$added)],
      folders$changed[!fs::is_dir(folders$changed)]) |>
      lapply(function(item) get_path(importTopic, item)) |>
      unlist()
  }
  
  ## 扫描任务脚本文件夹
  if(!is_empty(batchFolders)) {
    batchFolders |>
      sort() |>
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
    warning("No Batch Folder need to Import!!!")
    tibble()
  }
}

#' @title 从导入素材中提取未扫描文件
#' @description 
#' 读取导入素材，同时读取导入状态库，
#' 将未曾导入或有修改时间有变动的文件写入状态库。
#' 
#' 处理导入任务时也将修改这一状态库，标记todo、ignore等字段。
#' @family import function
#' @export
import_scan <- function(importDataset = "__IMPORT_FILES__",
                        importTopic = "IMPORT",
                        snapTopic = "SNAP",
                        cacheTopic = "CACHE") {
  changed_files <- import_changed(importTopic, snapTopic)

  if(!is_empty(changed_files)) {
    ## 筛查未曾入库或虽曾入库但已修改的素材文件
    existing <- ds_read(dsName = importDataset, topic = cacheTopic) |>
      collect()
    if(is_empty(existing)) {
      newScan <- changed_files
    } else {
      newScan <- changed_files |>
        anti_join(existing, by = c("filePath", "lastmodifiedAt"))
    }
    ## 入库
    if(nrow(newScan) > 0) {
      newScan |>
        mutate(todo = TRUE) |>
        ds_write(dsName = importDataset, topic = cacheTopic)
    } else {
      message("No new files to import!!")
    }
    ## 重新生成导入素材快照
    get_path("IMPORT") |>
      fileSnapshot(md5sum = TRUE, recursive = F) |>
      saveRDS(get_path(snapTopic, "import.rds"))
  }
}

#' @title 扫描所有未处理、未忽略的文件
#' @family import function
#' @export
import_search <- function(fileMatch = ".*",
                         batchMatch = ".*",
                         importDataset = "__IMPORT_FILES__",
                         cacheTopic = "CACHE",
                         ignoreFlag = FALSE,
                         todoFlag = TRUE) {
  filesToRead <- ds_read(dsName = importDataset, topic = cacheTopic)
  if(!is_empty(filesToRead)) {
    d0 <- filesToRead |> filter(todo %in% todoFlag)
    d0 |> filter(ignore == ignoreFlag) |>
      collect() |>
      filter(stringr::str_detect(filePath, fileMatch)) |>
      filter(stringr::str_detect(batchFolder, batchMatch)) |>
      mutate(fileSize = fs::as_fs_bytes(fileSize))
  } else {
    tibble()
  }
}

#' @title 定义导入任务
#' @family import function
#' @export
import_task_create <- function(taskId,
                               importFileMatch,
                               targetDataset,
                               importTopic = "IMPORT",
                               cacheTopic = "CACHE",
                               taskTopic = "TASK_DEFINE",
                               scriptsTopic = "TASK_SCRIPTS") {
  task_create(taskId = modelId,
              taskType = "__IMPORT__",
              taskTopic = taskTopic,
              extention = list(
                importFileMatch = importFileMatch,
                importTopic = importTopic,
                targetDataset = targetDataset,
                cacheTopic = cacheTopic))
}

#' @title 执行所有导入任务
#' @description 
#' 对于新素材，或指定素材文件，执行所有任务检查。
#' 
#' @family import function
#' @export
import_run <- function(files = tibble(),
                       tasks = tibble(),
                       toImport = TRUE,
                       taskTopic = "TASK_DEFINE",
                       importTopic = "IMPORT",
                       importDataset = "__IMPORT_FILES__",
                       cacheTopic = "CACHE") {
  ##
  filesToRead <- files %empty% import_search(importDataset = importDataset, cacheTopic = cacheTopic)
  tasksToMatch <- tasks %empty% task_search(typeMatch = "IMPORT", taskTopic = taskTopic)

  ## 按照约定，使用taskId匹配导入素材的filePath
  if(!is_empty(filesToRead)) {
    if(!is_empty(tasksToMatch)) {
      hasMatched <- 0
      resp <- tasksToMatch |>
        filter(online) |>
        select(taskTopic, taskId) |>
        purrr::pmap(function(taskTopic, taskId) {
          pat <- paste0("^", taskId, "[./]")
          matched <- filesToRead |>
            filter(stringr::str_detect(filePath, pat))
          message("Task Define", " <", taskTopic, ": ", taskId, "> matched ",  nrow(matched), " files !!")
          ## 如果任务可以匹配到导入素材，则执行该任务
          if(nrow(matched) > 0) {
            if(toImport) {
              task_run(taskId = taskId,
                       taskTopic = taskTopic,
                       importTopic = importTopic,
                       filesMatched = matched)
              matched |>
                mutate(todo = FALSE, doneAt = now(tzone = "Asia/Shanghai")) |>
                ds_append(importDataset, cacheTopic)
            }
            hasMatched <<- hasMatched + nrow(matched)
          }
          list("taskId" = taskId,
               "taskTopic" = taskTopic,
               "importTopic" = importTopic,
               "filesMatched" = matched)
        })
      if(hasMatched == 0) {
        message("Zero import files to MATCH task defined!!")
      } else {
        ds_submit(importDataset, topic = cacheTopic)
      }
      resp
    } else {
      warning("No Tasks been selected!!")
      tibble()
    }
  } else {
    warning("No Import Files been selected!!")
    tibble()
  }
}
