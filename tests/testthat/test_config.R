library(dplyr)
library(tibble)

test_that("加载YAML配置文件", {
  p <- tempdir()
  config_init(p)
  config_yaml(p)$IMPORT |> testthat::expect_equal("./IMPORT")

  get_topic("IMPORT") |>
    testthat::expect_equal(fs::path_join(c(p, "IMPORT")))
  get_topic("CACHE") |>
    testthat::expect_equal(fs::path_join(c(p, "CACHE")))
  remove_dir(p)
})

test_that("补充配置项", {
  p <- tempdir()
  fs::dir_create(p)
  config_init(p, option = list("RISK" = "./RISK"))
  config_yaml(p)$RISK |> testthat::expect_equal("./RISK")
  
  get_topic("RISK") |>
    testthat::expect_equal(fs::path_join(c(p, "RISK")))

  ## 补写  
  config_write(p, option = list("ABC" = "./ABC"))
  config_load(p)
  get_topic("RISK") |>
    testthat::expect_equal(fs::path_join(c(p, "RISK")))
  get_topic("ABC") |>
    testthat::expect_equal(fs::path_join(c(p, "ABC")))
  
  remove_dir(p)
})
