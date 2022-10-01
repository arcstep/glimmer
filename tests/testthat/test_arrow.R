library(dplyr, warn.conflicts = F)
library(tibble, warn.conflicts = F)

set_topic("STATE", "/tmp/glimmer/STATE")
set_topic("CACHE", "/tmp/glimmer/CACHE")

clear_dir <- function() {
  get_path("CACHE") |> remove_dir()
  get_path("STATE") |> remove_dir()
}

## delete_matching 模式会覆盖涉及到的分区
## 但不会删除数据中没有涉及到的分区目录
test_that("重写数据中包含的分区目录", {
  ds_remove_path("车数据")
  mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl)) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "delete_matching")
  ds_read("车数据")$files |> length() |>
    testthat::expect_equal(3)

  mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl)) |>
    filter(cyl != 8) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      partitioning = "cyl",
      version = "2.0",
      basename_template = "abc-{i}.p",
      existing_data_behavior = "delete_matching")
  ds_read("车数据")$files |> length() |>
    testthat::expect_equal(3)
  
  clear_dir()
})

## overwrite 模式会覆盖同名文件
## 但不会删除名字不同的数据文件，即使分区相同
test_that("重写数据中包含的所有分区内文件", {
  ds_remove_path("车数据")
  mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl)) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "delete_matching")
  ds_read("车数据")$files |> length() |>
    testthat::expect_equal(3)
  
  mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl)) |>
    filter(cyl != 8) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      partitioning = "cyl",
      version = "2.0",
      basename_template = "abcd-{i}.p",
      existing_data_behavior = "overwrite")
  ds_read("车数据")$files |> length() |>
    testthat::expect_equal(5)
  
  clear_dir()
})

## 局部写入数据时，如果列格式不同会发生什么？
test_that("当新分区数据比旧数据少一列时，数据集结构将被更新", {
  ds_remove_path("车数据")
  mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl)) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "delete_matching")
  ds_read("车数据") |> head() |> length() |>
    testthat::expect_equal(11)

  mtcars |> as_tibble() |>
    select(-3) |>
    mutate(cyl = as.integer(cyl)) |>
    filter(cyl != 8) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      partitioning = "cyl",
      version = "2.0",
      basename_template = "abcd-{i}.p",
      existing_data_behavior = "delete_matching")
  ds_read("车数据") |> head() |> length() |>
    testthat::expect_equal(10)

  clear_dir()
})

test_that("当新分区数据比旧数据多一列时，数据集结构也将被更新", {
  ds_remove_path("车数据")
  mtcars |> as_tibble() |>
    select(-3) |>
    mutate(cyl = as.integer(cyl)) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "delete_matching")
  ds_read("车数据") |> head() |> length() |>
    testthat::expect_equal(10)
  
  mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl)) |>
    filter(cyl != 8) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      partitioning = "cyl",
      version = "2.0",
      basename_template = "abcd-{i}.p",
      existing_data_behavior = "delete_matching")
  ds_read("车数据") |> head() |> length() |>
    testthat::expect_equal(11)
  
  clear_dir()
})


##
test_that("将列分为两组存储，合并读取时仍然支持惰性操作", {
  ds_remove_path("车数据")
  ds_remove_path("车数据补充")
  mtcars |>
    rownames_to_column() |>
    select(1:3) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      version = "2.0",
      existing_data_behavior = "delete_matching")
  mtcars |>
    rownames_to_column() |>
    select(1, 4:5) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据补充"),
      format = "parquet",
      version = "2.0",
      existing_data_behavior = "delete_matching")
  ## 惰性操作读取
  left_join(
    ds_read("车数据"),
    ds_read("车数据补充") |> head(5),
    by = "rowname") |>
    filter(cyl != 4) |>
    collect() |>
    length() |>
    testthat::expect_equal(5)

  clear_dir()
})
