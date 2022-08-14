library(dplyr)
library(tibble)

set_topic("RISKMODEL", "/tmp/glimmer/RISKMODEL")
set_topic("CACHE", "/tmp/glimmer/CACHE")
set_topic("STATE", "/tmp/glimmer/STATE")

if(fs::dir_exists("/tmp/glimmer/RISKMODEL")) {
  fs::dir_delete(get_path("RISKMODEL"))
}
if(fs::dir_exists("/tmp/glimmer/CACHE")) {
  fs::dir_delete(get_path("CACHE"))
}
if(fs::dir_exists("/tmp/glimmer/STATE")) {
  fs::dir_delete(get_path("STATE"))
}

test_that("创建风险模型", {
  risk_model_create("问题车辆", "车辆数据")
  fs::file_exists("/tmp/glimmer/RISKMODEL/问题车辆.yml") |>
    as.logical() |>
    testthat::expect_equal(TRUE)

  risk_model_create("问题车辆/耗油车型", "车辆数据")
  fs::file_exists("/tmp/glimmer/RISKMODEL/问题车辆/耗油车型.yml") |>
    as.logical() |>
    testthat::expect_equal(TRUE)

  risk_model_create("问题车辆/耗油车型", "车辆数据") |>
    as.logical() |>
    testthat::expect_warning("Existing")
})

test_that("执行模型", {
  mtcars |> as_tibble() |> ds_write("车辆数据")
  risk_model_create(
    modelName = "问题车辆/耗油车型",
    dataset = "车辆数据",
    filter = list(list(column = "mpg", op = ">", value = 20)),
    overwrite = TRUE)
  risk_model_run()

})
