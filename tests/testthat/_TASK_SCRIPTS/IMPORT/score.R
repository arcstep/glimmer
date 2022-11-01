#' @title 导入score数据
#' @param importTopic IMPORT
#' @param filesMatched 导入素材文件名

if(!rlang::is_empty(filesMatched)) {
  filesMatched |>
    select(batchFolder, filePath) |>
    purrr::pwalk(function(batchFolder, filePath) {
      f <- get_path(importTopic, batchFolder, filePath)
      f |>
        readr::read_csv() |>
        ds_as_from(f) |>
        ds_append("score")
  })
  ds_submit("score")
}