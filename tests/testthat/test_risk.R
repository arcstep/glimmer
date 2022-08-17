library(dplyr)
library(tidyr)
library(tibble)

set_topic("RISKMODEL", "/tmp/glimmer/RISKMODEL")
set_topic("CACHE", "/tmp/glimmer/CACHE")
set_topic("STATE", "/tmp/glimmer/STATE")

clear_dir <- function() {
  get_path("RISKMODEL") |> remove_dir()
  get_path("CACHE") |> remove_dir()
  get_path("STATE") |> remove_dir()
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
  
  clear_dir()
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
  
  clear_dir()
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

  clear_dir()
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
  
  clear_dir()
})

test_that("运行模型：时间和日期的逻辑", {
  d <- tibble(
    n = 1:5,
    day = c("2020-05-01", "2020-06-01", "2020-05-11", "2020-07-3", "2019-12-30") |> lubridate::as_date(),
    dt = c("2020-05-01 11:11:11", "2020-06-01 11:11:11", "2020-05-11 11:11:11", "2020-07-3 11:11:11", "2019-12-30 11:11:11") |> lubridate::as_datetime(tz = "Asia/Shanghai")
    )
  d |> mutate(keyId = row_number()) |>
    ds_write("胜利日", keyColumns = "keyId", titleColumn = "day")
  
  risk_model_create(
    modelName = "胜利日在7月",
    dataset = "胜利日",
    riskTip = "在7月",
    level = "L",
    filter = list(
      list(column = "dt", op = "%time%>=", value = c("2020-07-1"))))
  risk_model_run(modelName = "胜利日在7月", batchNumber = 1L)
  risk_data_read("疑点数据") |> 
    filter(batchNumber == 1L) |> nrow() |>
    testthat::expect_equal(
      d |> filter(dt >= lubridate::as_datetime("2020-07-1", tz = "Asia/Shanghai")) |> nrow())

  risk_model_create(
    modelName = "胜利日在7月2",
    dataset = "胜利日",
    riskTip = "在7月",
    level = "L",
    filter = list(
      list(column = "day", op = "%date%>=", value = c("2020-07-1"))))
  risk_model_run(modelName = "胜利日在7月2", batchNumber = 2L)
  risk_data_read("疑点数据") |> 
    filter(batchNumber == 2L) |> nrow() |>
    testthat::expect_equal(
      d |> filter(dt >= lubridate::as_date("2020-07-1")) |> nrow())
  
  clear_dir()
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
  
  clear_dir()
})

# 运行模型：支持二元操作过滤条件，包括数值、时间和文本
# 二元操作可使用多重条件组合过滤实现，不必专门实现
# 多重与条件：在一个模型中使用多重条件
# 多重或条件：将逻辑写入多个模型中

test_that("运行模型：在同一组中使用多个模型，不重复生成疑点数据", {
  d <- iris |> as_tibble()
  d |> as_tibble() |>
    mutate(keyId = row_number()) |>
    unite(title, c("Species", "Sepal.Length"), sep = "-", remove = FALSE) |>
    ds_write("iris", keyColumns = "keyId", titleColumn = "title")
  
  risk_model_create(
    modelName = "鸾尾花/萼片大1",
    modelGroup = "鸾尾花",
    dataset = "iris",
    riskTip = "够大",
    level = "L",
    filter = list(list(column = "Sepal.Length", op = ">", value = 6)),
    overwrite = TRUE)
  risk_model_create(
    modelName = "鸾尾花/萼片大2",
    modelGroup = "鸾尾花",
    dataset = "iris",
    riskTip = "够大",
    level = "L",
    filter = list(list(column = "Sepal.Width", op = ">", value = 3)),
    overwrite = TRUE)
  risk_model_run(modelName = "鸾尾花/萼片大1", batchNumber = 1)
  risk_model_run(modelName = "鸾尾花/萼片大2", batchNumber = 1)
  risk_data_read("疑点数据") |>
    filter(batchNumber == 1) |>
    nrow() |>
    testthat::expect_equal(
      d |> filter(Sepal.Length > 6 | Sepal.Width > 3) |> nrow())
  
  clear_dir()
})

test_that("运行模型：不同组的不同模型，应重复生成疑点数据", {
  d <- iris |> as_tibble()
  d |> as_tibble() |>
    mutate(keyId = row_number()) |>
    unite(title, c("Species", "Sepal.Length"), sep = "-", remove = FALSE) |>
    ds_write("iris", keyColumns = "keyId", titleColumn = "title")
  
  risk_model_create(
    modelName = "鸾尾花/萼片大1",
    modelGroup = "萼片大1",
    dataset = "iris",
    riskTip = "够大",
    level = "L",
    filter = list(list(column = "Sepal.Length", op = ">", value = 6)),
    overwrite = TRUE)
  risk_model_create(
    modelName = "鸾尾花/萼片大2",
    modelGroup = "萼片大2",
    dataset = "iris",
    riskTip = "够大",
    level = "L",
    filter = list(list(column = "Sepal.Width", op = ">", value = 3)),
    overwrite = TRUE)
  risk_model_run(modelName = "鸾尾花/萼片大1", batchNumber = 1)
  risk_model_run(modelName = "鸾尾花/萼片大2", batchNumber = 1)
  risk_data_read("疑点数据") |>
    filter(batchNumber == 1) |>
    nrow() |>
    testthat::expect_equal(
      d |> filter(Sepal.Length > 6) |> nrow() + d |> filter(Sepal.Width > 3) |> nrow())
  
  clear_dir()
})

# 运行模型：支持是否启用模型
test_that("运行模型：支持是否启用模型", {
  d <- iris |> as_tibble()
  d |> as_tibble() |>
    mutate(keyId = row_number()) |>
    unite(title, c("Species", "Sepal.Length"), sep = "-", remove = FALSE) |>
    ds_write("iris", keyColumns = "keyId", titleColumn = "title")
  
  risk_model_create(
    modelName = "鸾尾花/萼片大1",
    modelGroup = "鸾尾花",
    online = FALSE,
    dataset = "iris",
    riskTip = "够大",
    level = "L",
    filter = list(list(column = "Sepal.Length", op = ">", value = 6)),
    overwrite = TRUE)
  risk_model_create(
    modelName = "鸾尾花/萼片大2",
    modelGroup = "鸾尾花",
    online = TRUE,
    dataset = "iris",
    riskTip = "够大",
    level = "L",
    filter = list(list(column = "Sepal.Width", op = ">", value = 3)),
    overwrite = TRUE)
  risk_model_run(modelName = "鸾尾花/萼片大1", batchNumber = 1)
  risk_model_run(modelName = "鸾尾花/萼片大2", batchNumber = 1)
  risk_data_read("疑点数据") |>
    filter(batchNumber == 1) |>
    nrow() |>
    testthat::expect_equal(
      d |> filter(Sepal.Width > 3) |> nrow())
  
  clear_dir()
})

test_that("运行模型：设置疑点数据状态到__TODO__", {
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
  risk_data_set("51", "鸾尾花", flag = "__TODO__")
  (risk_data_read("疑点数据") |> filter(dataId == "51") |> collect())$flag |>
    testthat::expect_equal("__TODO__")

  clear_dir()
})

test_that("停用模型：清理已经生成的疑点数据", {
  d <- iris |> as_tibble()
  d |> as_tibble() |>
    mutate(keyId = row_number()) |>
    unite(title, c("Species", "Sepal.Length"), sep = "-", remove = FALSE) |>
    ds_write("iris", keyColumns = "keyId", titleColumn = "title")
  
  risk_model_create(
    modelName = "鸾尾花/萼片大1",
    modelGroup = "鸾尾花",
    online = TRUE,
    dataset = "iris",
    riskTip = "够大",
    level = "L",
    filter = list(list(column = "Sepal.Length", op = ">", value = 6)),
    overwrite = TRUE)
  risk_model_create(
    modelName = "鸾尾花/萼片大2",
    modelGroup = "鸾尾花",
    online = TRUE,
    dataset = "iris",
    riskTip = "够大",
    level = "L",
    filter = list(list(column = "Sepal.Width", op = ">", value = 3)),
    overwrite = TRUE)
  risk_model_run(modelName = "鸾尾花/萼片大1", batchNumber = 1)
  risk_model_run(modelName = "鸾尾花/萼片大2", batchNumber = 1)
  risk_data_set(dataId = "51", modelGroup = "鸾尾花", flag = "__TODO__")
  risk_data_clear(modelName = "鸾尾花/萼片大1", modelGroup = "鸾尾花")
  risk_data_read("疑点数据") |>
    filter(modelName == "鸾尾花/萼片大1") |>
    nrow() |>
    testthat::expect_equal(1)

  risk_data_set(dataId = "51", modelGroup = "鸾尾花", flag = "__NEW__")
  risk_data_clear(modelName = "鸾尾花/萼片大1", modelGroup = "鸾尾花")
  risk_data_read("疑点数据") |>
    filter(modelName == "鸾尾花/萼片大1") |>
    nrow() |>
    testthat::expect_equal(0)
  
  clear_dir()
})

