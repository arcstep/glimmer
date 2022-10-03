library(dplyr, warn.conflicts = F)
library(tibble, warn.conflicts = F)

config_init(tempdir())

clear_dir <- function() {
  get_path("CACHE") |> remove_dir()
}

test_that("当数据集配置文件不存在时", {
  ds_remove_path("车数据")
  ds_append(mtcars, "车数据") |>
    testthat::expect_error("Empty Dataset Metadata")
})

test_that("当新数据集结构与配置文件不一致", {
  ds_remove_path("车数据")
  ds_init("车数据", data = mtcars |> head())
  
  mtcars |> select(-2) |> ds_append("车数据") |>
    testthat::expect_error("Different Schema")
  
  mtcars |> mutate("xx" = "XX") |> ds_append("车数据") |>
    testthat::expect_error("Different Schema")
})

test_that("追加数据：创建新数据", {
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_remove_path("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> slice(1:10) |> as_tibble() |> ds_append("车数据")
  ds_read("车数据") |> collect() |> nrow() |>
    testthat::expect_equal(10)
  
  m |> slice(1:10) |> as_tibble() |> ds_append("车数据")
  get_path("CACHE", "车数据") |> arrow::open_dataset() |> nrow() |>
    testthat::expect_equal(20)
  ds_read("车数据") |> collect() |> nrow() |>
    testthat::expect_equal(10)
})

test_that("删除数据：没有设置主键时不允许删除", {
  ## 没有设置主键，删除失败
  ds_remove_path("车数据")
  ds_init("车数据", data = mtcars |> head())
    mtcars |> slice(1:10) |> as_tibble() |> ds_append("车数据")
  mtcars |> slice(1:3) |> as_tibble() |> ds_delete("车数据") |>
    testthat::expect_error("Can't Delete without keyColumns")

  ## 设置了主键，删除成功
  ds_remove_path("车数据")
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  m |> slice(1:10) |> as_tibble() |> ds_append("车数据")
  m |> slice(1:3) |> as_tibble() |> ds_delete("车数据")
    ds_read("车数据") |> collect() |> nrow() |>
    testthat::expect_equal(7)
})

test_that("归档数据操作：无分区", {
  ## 设置了主键，删除成功
  ds_remove_path("车数据")
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  m |> slice(1:10) |> as_tibble() |> ds_append("车数据")
  m |> slice(1:3) |> as_tibble() |> ds_delete("车数据")
  
  ds_submit("车数据")
  ds_read("车数据") |> collect() |> nrow() |>
    testthat::expect_equal(7)
})

test_that("归档数据操作：有分区", {
  ## 设置了主键，删除成功
  ds_remove_path("车数据")
  m <- mtcars |> as_tibble() |> rownames_to_column() |> mutate(cyl = as.integer(cyl))
  ds_init("车数据", keyColumns = "rowname", partColumns = c("cyl"), data = m |> head())
  m |> slice(1:10) |> as_tibble() |> ds_append("车数据")
  m |> slice(6:15) |> as_tibble() |> ds_append("车数据")
  m |> slice(1:3) |> as_tibble() |> ds_delete("车数据")
  
  ds_submit("车数据")
  ds_read("车数据") |> collect() |> nrow() |>
    testthat::expect_equal(10+5-3)
  
  m |> filter(cyl == 4) |> as_tibble() |> ds_append("车数据")
  ds_submit("车数据")
  ds_read("车数据") |> collect() |> nrow() |>
    testthat::expect_equal(20)
})
