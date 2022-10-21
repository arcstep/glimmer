#' @title 导入score数据
#' @param importTopic IMPORT
#' @param files 导入素材文件名

if(!rlang::is_empty(files)) {
  files |> purrr::walk(function(f) {
    readr::read_csv(get_path(importTopic, f)) |>
      mutate(`@from` = f) |>
      ds_append("score")
  })
  ds_submit("score")
}