library(dplyr, warn.conflicts = F)
library(tibble, warn.conflicts = F)

test_that("写入一个简单的文件", {
  rootPath <- tempdir()
  config_init(rootPath)
  
  ds_remove_path("车数据")
  list("dsName" = "车数据") |> ds_yaml_write(dsName = "车数据")
  ds_yaml("车数据")$dsName |> testthat::expect_equal("车数据")
  
  list("partColumns" = c("cyl", "am")) |> ds_yaml_write(dsName = "车数据")
  x <- ds_yaml("车数据") |> names()
  testthat::expect_equal(c("dsName", "partColumns") %in% x, c(T, T))

  list("dsName" = "我的车") |> ds_yaml_write(dsName = "车数据")
  ds_yaml("车数据")$dsName |> testthat::expect_equal(c("我的车"))
  
  rootPath |> fs::dir_delete()
})
