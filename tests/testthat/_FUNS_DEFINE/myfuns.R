
## 普通函数准备
mycars <- function() { mtcars |> as_tibble() }
mycyl <- function(i_cyl = 4) { mtcars |> as_tibble() |> filter(cyl == i_cyl) }
myarrange <- function(d, sv_columns) {
  d |> arrange(!!!rlang::syms(sv_columns))
}

## 预定义函数准备
gali_import_cars <- function() { mtcars |> as_tibble() }
gali_ds_filter_cyl <- function(d, i_cyl = 4) { d |> as_tibble() |> filter(cyl == i_cyl) }
gali_ds_as_sort <- function(d, sv_columns) {
  d |> arrange(!!!syms(sv_columns))
}

