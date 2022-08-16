library(dplyr)
library(tidyr)
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
  d <- mtcars |> as_tibble() |> filter(mpg <= 20)
  
  mtcars |> as_tibble() |>
    mutate(keyId = row_number()) |>
    ds_write("车辆数据", keyColumns = "keyId", titleColumn = "mpg")
  risk_model_create(
    modelName = "问题车辆/耗油车型",
    dataset = "车辆数据",
    riskTip = "耗油大，每加仑不足20公里",
    level = "L",
    filter = list(list(column = "mpg", op = "<=", value = 20)),
    overwrite = TRUE)
  risk_model_run(modelName = "问题车辆/耗油车型", batchNumber = 1)
  r <- risk_data_read("疑点数据")
  r |>
    filter(batchNumber == 1) |>
    collect() |>
    nrow() |>
    testthat::expect_equal(nrow(d))
  c("dataId", "dataTitle", "riskLevel", "value", "riskTip", "runAt",
    "modelName", "batchNumber", "dataset", "modelGroup") %in% names(r) |>
    purrr::walk(function(item) testthat::expect_true(item))
  
  fs::dir_delete(get_path("RISKMODEL"))
  fs::dir_delete(get_path("CACHE"))
})

test_that("运行模型：支持一元操作过滤条件", {
  d <- iris |> as_tibble()
  d |> as_tibble() |>
    mutate(keyId = row_number()) |>
    unite(title, c("Species", "Sepal.Length"), sep = "-", remove = FALSE) |>
    ds_write("iris", keyColumns = "keyId", titleColumn = "title")
  
  risk_model_create(
    modelName = "鸾尾花/setosa",
    dataset = "iris",
    riskTip = "setosa这种类型有问题",
    level = "L",
    filter = list(
      list(column = "Species", op = "==", value = "setosa")),
    overwrite = TRUE)
  risk_model_run(modelName = "鸾尾花/setosa", batchNumber = 1)
  risk_data_read("疑点数据") |> 
    filter(batchNumber == 1) |> nrow() |>
    testthat::expect_equal(
      d |> filter(Species == "setosa") |> nrow())

  fs::dir_delete(get_path("RISKMODEL"))
  fs::dir_delete(get_path("CACHE"))
})

test_that("运行模型：文本多选、正则表达式", {
  d <- iris |> as_tibble()
  d |> as_tibble() |>
    mutate(keyId = row_number()) |>
    unite(title, c("Species", "Sepal.Length"), sep = "-", remove = FALSE) |>
    ds_write("iris", keyColumns = "keyId", titleColumn = "title")
  
  risk_model_create(
    modelName = "鸾尾花1",
    dataset = "iris",
    riskTip = "setosa和virginica",
    level = "L",
    filter = list(
      list(column = "Species", op = "%in%", value = c("setosa", "virginica"))),
    overwrite = TRUE)
  risk_model_run(modelName = "鸾尾花1", batchNumber = 1)
  risk_data_read("疑点数据") |> 
    filter(batchNumber == 1) |> nrow() |>
    testthat::expect_equal(
      d |> filter(Species %in% c("setosa", "virginica") ) |> nrow())

  risk_model_create(
    modelName = "鸾尾花2",
    dataset = "iris",
    riskTip = "setosa和virginica",
    level = "L",
    filter = list(
      list(column = "Species", op = "%nin%", value = c("setosa", "virginica"))),
    overwrite = TRUE)
  risk_model_run(modelName = "鸾尾花2", batchNumber = 2)
  risk_data_read("疑点数据") |> 
    filter(batchNumber == 2) |> nrow() |>
    testthat::expect_equal(
      d |> filter(Species %nin% c("setosa", "virginica") ) |> nrow())
  
  risk_model_create(
    modelName = "鸾尾花3",
    dataset = "iris",
    riskTip = "setosa和virginica",
    level = "L",
    filter = list(
      list(column = "Species", op = "%regex%", value = "a$")),
    overwrite = TRUE)
  risk_model_run(modelName = "鸾尾花3", batchNumber = 3)
  risk_data_read("疑点数据") |> 
    filter(batchNumber == 3) |> nrow() |>
    testthat::expect_equal(
      d |> filter(Species %in% c("setosa", "virginica") ) |> nrow())
  
  fs::dir_delete(get_path("RISKMODEL"))
  fs::dir_delete(get_path("CACHE"))
})

# 运行模型：支持多重组合过滤条件
test_that("运行模型：支持多重组合过滤条件", {
  d <- iris |> as_tibble()
  d |> as_tibble() |>
    mutate(keyId = row_number()) |>
    unite(title, c("Species", "Sepal.Length"), sep = "-", remove = FALSE) |>
    ds_write("iris", keyColumns = "keyId", titleColumn = "title")
  
  risk_model_create(
    modelName = "鸾尾花/萼片大",
    dataset = "iris",
    riskTip = "够大",
    level = "L",
    filter = list(
      list(column = "Sepal.Length", op = ">", value = 6),
      list(column = "Sepal.Width", op = ">", value = 3)),
    overwrite = TRUE)
  risk_model_run(modelName = "鸾尾花/萼片大", batchNumber = 1)
  risk_data_read("疑点数据") |>
    filter(batchNumber == 1) |>
    nrow() |>
    testthat::expect_equal(
      d |> filter(Sepal.Length > 6 & Sepal.Width > 3) |> nrow())
  
  fs::dir_delete(get_path("RISKMODEL"))
  fs::dir_delete(get_path("CACHE"))
})

# 运行模型：支持二元操作过滤条件，包括数值、时间和文本


# 运行模型：按风险模型的模板描述自动生成，包括静态模板和嵌入value值的模板

# 运行模型：支持是否启用模型

# 停用模型：清理已经生成的疑点数据

# 运行模型：生成疑点数据的结构符合要求

# 重运行：已经生成的疑点数据不再重新生成



