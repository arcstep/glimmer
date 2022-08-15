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

test_that("创建模型：支持覆盖", {
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

test_that("运行模型：要求数据集具有关键列", {
  mtcars |> as_tibble() |>
    mutate(keyId = row_number()) |>
    ds_write("车辆数据", keyColumns = "keyId", titleColumn = "mpg")
  risk_model_create(
    modelName = "问题车辆/耗油车型",
    dataset = "车辆数据",
    filter = list(list(column = "mpg", op = "<=", value = 20, riskTip = "耗油大，每加仑不足20公里", level = 1)),
    overwrite = TRUE)
  risk_model_run("问题车辆/耗油车型")
  
})

# 运行模型：支持一元操作过滤条件，包括数值、时间和文本

# 运行模型：支持二元操作过滤条件，包括数值、时间和文本

# 运行模型：支持多重组合过滤条件

# 运行模型：按风险模型的模板描述自动生成，包括静态模板和嵌入value值的模板

# 运行模型：支持是否启用模型

# 运行模型：生成疑点数据的结构符合要求

# 未更新、重运行：已经生成的疑点数据不再重新生成

# 未更新、重运行：属于同一模型组的模型，当作同一模型

# 更新后、重运行：已经生成、但未处理的疑点数据，应当重新生成

