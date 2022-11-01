#' @title 遇到异常填充默认值
#' @family utils function
#' @export
`%error%` <- function(exp, defaultValue) {
  tryCatch({
    exp
  }, error = function(e) defaultValue)
}

#' @title 条件过滤：使用正则表达式
#' @family utils function
#' @export
`%regex%` <- function(a, b) {
  stringr::str_detect(a, b)
}

#' @title 条件过滤：使用正则表达式取反
#' @family utils function
#' @export
`%not-regex%` <- function(a, b) {
  stringr::str_detect(a, b, negate = TRUE)
}

#' @title 条件过滤：取反
#' @family utils function
#' @export
`%nin%` <- Negate(`%in%`)

#' @title 遇到空值填充默认值
#' @description 当值为NULL、空向量、空列表、空字符串时设置默认值
#' @family utils function
#' @export
`%empty%` <- function(a, b) {
  if (is_empty(a)) b else a
}

#' @title 遇到非空值填充默认值
#' @family utils function
#' @export
`%not-empty%` <- function(a, b) {
  if (!is_empty(a)) b else a
}

#' @title 遇到NA填充默认值
#' @family utils function
#' @export
`%na%` <- function(a, b) {
  if(identical(NA, a)) b else a
}

#' @title 遇到NA填充默认值
#' @family utils function
#' @export
`%not-na%` <- function(a, b) {
  if(identical(NA, a)) a else b
}

#' @title 删除文件夹
#' @family utils function
#' @export
remove_dir <- function(path) {
  if(fs::dir_exists(path)) {
    fs::dir_delete(path)
  }
}

#' @title 创建文件夹
#' @family utils function
#' @export
create_dir <- function(path) {
  if(!fs::dir_exists(path)) {
    fs::dir_create(path)
  }
}

#' @title 获取批次号
#' @family utils function
#' @export
gen_batchNum <- function() {
  n <- sprintf("%03d", sample(0:999, 1))
  m <- lubridate::now() |> as.character.Date(format = "SN-%Y-%m%d-%H%M%S")
  paste(m, n, sep = "-")
}
