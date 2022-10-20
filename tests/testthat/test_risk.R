test_that("创建模型：一般情况", {
  sample_config_init()
  import_init()
  task_queue_init()
  risk_data_init()
  m <- mtcars |> as_tibble() |> rowid_to_column()
  ds_init("cars", data = m, type = "__BUILD__", keyColumns = "rowid", titleColumn = "cyl")
  m |> ds_write("cars")
  
  ## 构造风险模型
  risk_model_create("cars", modelName = "risk/cyl-too-big") |>
    task_item_gali_add(gali_dataset_filter("cyl", ">=", 8))
  ("risk/cyl-too-big/main" |> task_run())$cyl |> unique() |>
    testthat::expect_equal(8)

  ## 写入风险疑点数据
  risk_model_create("cars", modelName = "risk/cyl-middle") |>
    task_item_gali_add(gali_dataset_filter("cyl", ">=", 6)) |>
    task_item_gali_add(gali_dataset_filter("cyl", "<", 8)) |>
    risk_data_build()
  
  "risk/cyl-middle/main" |> task_run()
  (risk_data_read() |> distinct(dataTitle))$dataTitle |>
    testthat::expect_equal("6")

  temp_remove()  
})

test_that("创建模型：要求数据集有关键列和主题列", {
  sample_config_init()
  import_init()
  task_queue_init()
  risk_data_init()
  m <- mtcars |> as_tibble() |> rowid_to_column()
  ds_init("cars1", data = m, type = "__BUILD__", titleColumn = "cyl")
  ds_init("cars2", data = m, type = "__BUILD__", keyColumns = "rowid")

  ## 构造风险模型
  risk_model_create("cars1", modelName = "risk/cars1/cyl-too-big") |>
    testthat::expect_error("No KeyColumns")
  risk_model_create("cars2", modelName = "risk/cars2/cyl-too-big") |>
    testthat::expect_error("No TitleColumn")

  temp_remove()
})

test_that("疑点数据：清理未处理的疑点数据", {
  sample_config_init()
  import_init()
  task_queue_init()
  risk_data_init()
  m <- mtcars |> as_tibble() |> rowid_to_column()
  ds_init("cars", data = m, type = "__BUILD__", keyColumns = "rowid", titleColumn = "cyl")
  m |> ds_write("cars")
  
  ## 写入风险疑点数据
  risk_model_create("cars", modelName = "risk/cyl-middle") |>
    task_item_gali_add(gali_dataset_filter("cyl", ">=", 6)) |>
    task_item_gali_add(gali_dataset_filter("cyl", "<", 8)) |>
    risk_data_build()
  
  "risk/cyl-middle/main" |> task_run()
  risk_data_read() |> nrow() |>
    testthat::expect(7)
  
  risk_data_read() |>
    filter(dataId == "1") |>
    mutate(doneAt = now()) |>
    ds_write("__RISK_DATA__")
  risk_data_read() |> nrow() |>
    testthat::expect(6)

  temp_remove()
})

