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
})

test_that("移除旧文件", {
})

test_that("更新部分文件分区", {
})

test_that("使用关键列去重", {
})

test_that("当关键列所在分区被修改", {
})

