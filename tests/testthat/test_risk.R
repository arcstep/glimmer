test_that("创建模型：一般情况", {
  sample_init()
  m <- mtcars |> as_tibble() |> rowid_to_column()
  ds_init("cars", data = m, type = "__BUILD__", keyColumns = "rowid", titleColumn = "cyl")
  m |> ds_write("cars")
  
  ## 构造风险模型
  resp <- task_risk_model_create("cars", modelName = "risk/cyl-too-big") |>
    script_func_add("ds_filter", list(column = "cyl", op = ">=", value = 8)) |>
    script_func_add("ds_collect") |>
    task_run()
  resp$cyl |> unique() |>
    testthat::expect_equal(8)

  ## 写入风险疑点数据
  task_risk_model_create("cars", modelName = "risk/cyl-middle") |>
    script_func_add("ds_filter", list(column = "cyl", op = ">=", value = 6)) |>
    script_func_add("ds_filter", list(column = "cyl", op = "<", value = 8)) |>
    script_func_add("risk_data_write") |>
    task_run()
  (risk_data_read() |> distinct(dataTitle))$dataTitle |>
    testthat::expect_equal("6")

  temp_remove()  
})

test_that("创建模型：要求数据集有关键列和主题列", {
  sample_init()
  m <- mtcars |> as_tibble() |> rowid_to_column()
  ds_init("cars1", data = m, type = "__BUILD__", titleColumn = "cyl")
  ds_init("cars2", data = m, type = "__BUILD__", keyColumns = "rowid")

  ## 构造风险模型
  task_risk_model_create("cars1", modelName = "risk/cars1/cyl-too-big") |>
    testthat::expect_error("No KeyColumns")
  task_risk_model_create("cars2", modelName = "risk/cars2/cyl-too-big") |>
    testthat::expect_error("No TitleColumn")

  temp_remove()
})

test_that("疑点数据：清理未处理的疑点数据", {
  sample_init()
  m <- mtcars |> as_tibble() |> rowid_to_column()
  ds_init("cars", data = m, type = "__BUILD__", keyColumns = "rowid", titleColumn = "cyl")
  m |> ds_write("cars")
  
  ## 写入风险疑点数据
  task_risk_model_create("cars", modelName = "risk/cyl-middle") |>
    script_func_add("ds_filter", list(column = "cyl", op = ">=", value = 6)) |>
    script_func_add("ds_filter", list(column = "cyl", op = "<", value = 8)) |>
    script_func_add("risk_data_write") |>
    task_run()
  
  risk_data_read() |> nrow() |>
    testthat::expect_equal(7)
  
  risk_data_read() |> filter(dataId == "1") |> risk_data_set_done()
  risk_data_read() |> nrow() |>
    testthat::expect_equal(6)

  task_risk_model_create("cars", modelName = "risk/cyl-middle", tagName = "V2") |>
    script_func_add("ds_filter", list(column = "cyl", op = ">=", value = 6)) |>
    script_func_add("risk_data_write") |>
    task_run()
  
  risk_data_read() |> nrow() |>
    testthat::expect_equal(20)
  
  temp_remove()
})

