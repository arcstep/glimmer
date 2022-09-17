library(dplyr)
library(tibble)

test_that("设置目标文件夹", {
  set_topic("IMPORT", "/tmp/glimmer/IMPORT")
  get_topic("IMPORT") |> testthat::expect_equal("/tmp/glimmer/IMPORT")
  get_path("IMPORT", "abc") |> as.character() |> testthat::expect_equal("/tmp/glimmer/IMPORT/abc")
})

test_that("加载YAML配置文件：一般设置", {
  fs::dir_create("/tmp/glimmer/")
  list("IMPORT" = "/tmp/glimmer/IMPORT", "BUILD" = "/tmp/glimmer/BUILD") |>
    yaml::write_yaml("/tmp/glimmer/config.yml")
  
  load_config("/tmp/glimmer/config.yml")
  get_topic("IMPORT") |> as.character() |>
    testthat::expect_equal("/tmp/glimmer/IMPORT")
  get_topic("BUILD") |> as.character() |>
    testthat::expect_equal("/tmp/glimmer/BUILD")
  remove_dir("/tmp/glimmer")
})

test_that("加载YAML配置文件：使用全局变量", {
  fs::dir_create("/tmp/glimmer/")
  list(
    "ROOT_PATH" = "/tmp/glimmer",
    "__MY_LOVE__" = "ABC",
    "IMPORT" = "./IMPORT",
    "BUILD" = "./BUILD") |>
    yaml::write_yaml("/tmp/glimmer/config.yml")
  
  load_config("/tmp/glimmer/config.yml")
  get_topic("IMPORT") |> as.character() |>
    testthat::expect_equal("/tmp/glimmer/IMPORT")
  get_topic("BUILD") |> as.character() |>
    testthat::expect_equal("/tmp/glimmer/BUILD")
  remove_dir("/tmp/glimmer")
})