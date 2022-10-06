library(dplyr, warn.conflicts = F)
library(tibble, warn.conflicts = F)

config_init(tempdir())

clear_dir <- function() {
  get_path("CACHE") |> remove_dir()
}

test_that("内存中对数据去重", {
  all <- mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl), am = as.integer(am)) |>
    mutate(id = row_number())
  
  rbind(all |> slice(1:3), all |> slice(4:6)) |>
    ds_as_unique("id") |>
    nrow() |>
    testthat::expect_equal(6)
    
  rbind(all |> slice(1:3), all |> slice(3:5)) |>
    ds_as_unique("id") |>
    nrow() |>
    testthat::expect_equal(5)
  clear_dir()
})

test_that("读写数据时使用推荐的显示列", {
  ds_remove_path("车数据")
  all <- mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl), am = as.integer(am)) |>
    mutate(id = row_number())
  
  all |> glimmer::ds_write("车数据", suggestedColumns = c("id"))
  (glimmer::ds_read("车数据") |> names())[[1]] |>
    testthat::expect_equal("id")
  glimmer::ds_read("车数据") |> names() |> length() |>
    testthat::expect_equal(length(all |> names()))
  
  all |> glimmer::ds_write("车数据", suggestedColumns = c("id", "cyl"))
  (glimmer::ds_read("车数据") |> names())[[2]] |>
    testthat::expect_equal("cyl")
  
  clear_dir()
})