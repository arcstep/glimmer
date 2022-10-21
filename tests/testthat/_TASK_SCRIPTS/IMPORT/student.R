#' @title 导入sutdent数据
#' @param importTopic IMPORT
#' @param files 导入素材文件名

if(!rlang::is_empty(files)) {
  files |> purrr::walk(function(f) {
    readr::read_csv(get_path(importTopic, f)) |>
      ds_as_from(f) |>
      ds_append("student")
  })
  ds_submit("student")
}

