## %error% 取不到值时使用默认值
`%error%` <- function(exp, defaultValue) {
  tryCatch({
    exp
  }, error = function(e) defaultValue)
}

## %regex%正则表达式
`%regex%` <- function(a, b) {
  stringr::str_detect(a, b)
}

`%not-regex%` <- function(a, b) {
  stringr::str_detect(a, b, negate = TRUE)
}

## %in%取反
`%nin%` <- Negate(`%in%`)

## 当值为NULL、空向量、空列表、空字符串时设置默认值
`%empty%` <- function(a, b) {
  if (rlang::is_empty(a)) b else a
}

## %empty%取反
`%not-empty%` <- function(a, b) {
  if (!rlang::is_empty(a)) b else a
}

## 条件执行
`%true%` <- function(a, b) {
  if(a) b
}

`%false%` <- function(a, b) {
  if(!a) b
}
