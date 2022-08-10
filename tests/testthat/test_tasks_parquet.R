library(dplyr)
library(tibble)

set_topic("STATE", "/tmp/glimmer/STATE")
set_topic("CACHE", "/tmp/glimmer/CACHE")
ds_remove_path("write_dataset", topic = "STATE")

test_that("写入一个简单的文件", {
  ds_remove_path("车数据")
  mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl)) |>
    glimmer::write_dataset("车数据")
  read_dataset("车数据") |> nrow() |>
    expect_equal(nrow(mtcars))
})

test_that("按分区写入文件", {
  ds_remove_path("车数据")
  mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl)) |>
    glimmer::write_dataset("车数据", partColumns = c("cyl"))
  read_dataset("车数据") |> nrow() |>
    expect_equal(nrow(mtcars))
})

test_that("数据集准备异常时尝试写入", {
  ds_remove_path("车数据")
  tibble() |> glimmer::write_dataset("车数据") |>
    testthat::expect_error("Empty Dataset")
  
  c("abc") |> glimmer::write_dataset("车数据") |>
    testthat::expect_error("Not Tibble")
  
  mtcars |> filter(cyl == 1000) |> glimmer::write_dataset("车数据") |>
    testthat::expect_warning("No Content")
})

test_that("更新部分文件分区", {
  ds_remove_path("车数据")
  all <- mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl), am = as.integer(am))
  
  ## 提前写入一个分区数据
  all |> filter(cyl == 6 & am == 1) |>
    glimmer::write_dataset("车数据", partColumns = c("cyl", "am"))
  part1 <- (fs::file_info(get_path("CACHE", "车数据", "cyl=6/am=1/part-0.parquet")))$modification_time
  Sys.sleep(1)

  ## 将数据写入不重叠的另一个分区，此时不应更新旧文件
  all |> filter(cyl == 4 & am == 1) |>
    glimmer::write_dataset("车数据", partColumns = c("cyl", "am"))
  part2 <- (fs::file_info(get_path("CACHE", "车数据", "cyl=6/am=1/part-0.parquet")))$modification_time
  testthat::expect_equal(part1, part2)

  ## 将数据写入重叠的分区，此时应更新旧文件
  all |> filter(cyl == 6) |>
    glimmer::write_dataset("车数据", partColumns = c("cyl", "am"))
  part3 <- (fs::file_info(get_path("CACHE", "车数据", "cyl=6/am=1/part-0.parquet")))$modification_time
  testthat::expect_gt(part3, part1)
})

test_that("当关键列所在分区被修改", {
  ds_remove_path("车数据")
  all <- mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl), am = as.integer(am)) |>
    mutate(id = row_number())
  
  ## 提前写入一个分区数据
  all |> filter(cyl == 6 & am == 1) |>
    glimmer::write_dataset("车数据", partColumns = c("cyl", "am"), keyColumns = "id")
  part1 <- (fs::file_info(get_path("CACHE", "车数据", "cyl=6/am=1/part-0.parquet")))$modification_time
  Sys.sleep(1)
  
  ## 将数据写入不重叠的另一个分区，此时不应更新旧文件
  all |> filter(id %in% c(3:4)) |>
    glimmer::write_dataset("车数据", partColumns = c("cyl", "am"), keyColumns = "id")
  part2 <- (fs::file_info(get_path("CACHE", "车数据", "cyl=6/am=1/part-0.parquet")))$modification_time
  testthat::expect_equal(part1, part2)
  
  ## 根据分区写入重叠的分区，此时应更新旧文件
  all |> filter(cyl == 6) |>
    glimmer::write_dataset("车数据", partColumns = c("cyl", "am"), keyColumns = "id")
  part3 <- (fs::file_info(get_path("CACHE", "车数据", "cyl=6/am=1/part-0.parquet")))$modification_time
  testthat::expect_gt(part3, part1)
  
  ## 根据主键写入重复的分区，此时应更新旧文件
  all |> filter(id == 2) |>
    glimmer::write_dataset("车数据", partColumns = c("cyl", "am"), keyColumns = "id")
  part4 <- (fs::file_info(get_path("CACHE", "车数据", "cyl=6/am=1/part-0.parquet")))$modification_time
  testthat::expect_gt(part4, part1)
  
})

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
})

test_that("写入磁盘时对数据去重", {
  ds_remove_path("车数据")
  all <- mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl), am = as.integer(am)) |>
    mutate(id = row_number())
  
  ## 写入3条
  all |> filter(id %in% 1:3) |>
    glimmer::write_dataset("车数据", partColumns = c("cyl", "am"))
  glimmer::read_dataset("车数据") |> nrow() |> testthat::expect_equal(3)

  ## 不指定关键列重复写入，应有6条
  all |> filter(id %in% 1:3) |>
    glimmer::write_dataset("车数据", partColumns = c("cyl", "am"))
  glimmer::read_dataset("车数据") |> nrow() |> testthat::expect_equal(6)
  
  ## 指定关键列重复，应有3条
  all |> filter(id %in% 1:3) |>
    glimmer::write_dataset("车数据", partColumns = c("cyl", "am"), keyColumns = "id")
  glimmer::read_dataset("车数据") |> nrow() |> testthat::expect_equal(3)
  
  ## 指定关键列重复，应有4条
  all |> filter(id %in% 2:4) |>
    glimmer::write_dataset("车数据", partColumns = c("cyl", "am"), keyColumns = "id")
  glimmer::read_dataset("车数据") |> nrow() |> testthat::expect_equal(4)
})

