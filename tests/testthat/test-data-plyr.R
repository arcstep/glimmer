test_that("<dp_read / dp_collect>", {
  sample_config_init()
  
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> slice(1:10) |> as_tibble() |> ds_append("车数据")
  dp_read("车数据") |>
    task_run_dp(cacheTopic = "CACHE") |> collect() |> nrow() |>
    testthat::expect_equal(10)
  
  task_create("cars/dp_read") |>
    task_item_dp_add(dp_read("车数据")) |>
    task_item_dp_add(dp_collect()) |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(10)
  
  temp_remove()
})

test_that("<dp_filter>：一般流程", {
  sample_config_init()
  sample_import_files()

  ## 使用默认的 @result 获得返回值
  dp_filter("cyl", ">", 6) |>
    task_run_dp(`@result` = mtcars) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl > 6) |> nrow())

  ## 使用新的变量名
  dp_filter("cyl", ">", 6, dataName = "mydata") |>
    task_run_dp(`mydata` = mtcars) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl > 6) |> nrow())
  
  temp_remove()
})

test_that("<dp_filter>：比较操作符", {
  dp_filter("cyl", "<", 6) |>
    task_run_dp(`@result` = mtcars) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl < 6) |> nrow())
  
  dp_filter("cyl", ">=", 6) |>
    task_run_dp(`@result` = mtcars) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl >= 6) |> nrow())
  
  dp_filter("cyl", "<=", 6) |>
    task_run_dp(`@result` = mtcars) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl <= 6) |> nrow())

  dp_filter("cyl", "==", 6) |>
    task_run_dp(`@result` = mtcars) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl == 6) |> nrow())
  
  dp_filter("cyl", "!=", 6) |>
    task_run_dp(`@result` = mtcars) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl != 6) |> nrow())
})

test_that("<dp_filter>：正则表达式", {
  dp_filter("rowname", "%regex%", "Mazda") |>
    task_run_dp(`@result` = mtcars |> rownames_to_column()) |> nrow() |>
    testthat::expect_equal(
      mtcars |>
        rownames_to_column() |>
        filter(stringr::str_detect(rowname, "Mazda")) |>
        nrow())

  dp_filter("rowname", "%not-regex%", "Mazda") |>
    task_run_dp(`@result` = mtcars |> rownames_to_column()) |> nrow() |>
    testthat::expect_equal(
      mtcars |>
        rownames_to_column() |>
        filter(stringr::str_detect(rowname, "Mazda", negate = T)) |>
        nrow())
})

test_that("<dp_filter>：包含", {
  dp_filter("rowname", "%in%", c("Honda Civic", "Fiat 128")) |>
    task_run_dp(`@result` = mtcars |> rownames_to_column()) |> nrow() |>
    testthat::expect_equal(
      mtcars |>
        rownames_to_column() |>
        filter(rowname %in% c("Honda Civic", "Fiat 128")) |>
        nrow())

  dp_filter("rowname", "%nin%", c("Honda Civic", "Fiat 128")) |>
    task_run_dp(`@result` = mtcars |> rownames_to_column()) |> nrow() |>
    testthat::expect_equal(
      mtcars |>
        rownames_to_column() |>
        filter(!(rowname %in% c("Honda Civic", "Fiat 128"))) |>
        nrow())
  
})

test_that("<dp_filter>：比较时间和日期", {
  d <- tibble(
    n = 1:5,
    day = c("2020-05-01", "2020-06-01", "2020-05-11", "2020-07-3", "2019-12-30") |> lubridate::as_date(),
    dt = c("2020-05-01 11:11:11", "2020-06-01 11:11:11", "2020-05-11 11:11:11", "2020-07-3 11:11:11", "2019-12-30 11:11:11") |> lubridate::as_datetime(tz = "Asia/Shanghai")
  )
  
  ## 测试日期格式比较
  dp_filter("day", "date# >", "2020-05-30") |> task_run_dp(`@result` = d) |> nrow() |>
    testthat::expect_equal(
      d |> filter(day > as_date("2020-05-30")) |> nrow()
    )
  
  ## 测试时间格式比较
  dp_filter("dt", "time# >", "2020-05-30") |> task_run_dp(`@result` = d) |> nrow() |>
    testthat::expect_equal(
      d |> filter(dt > as_datetime("2020-05-30")) |> nrow()
    )
})

test_that("<dp_head/ dp_tail>", {
  sample_config_init()
  
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> as_tibble() |> ds_append("车数据")

  task_create("cars/dp_read") |>
    task_item_dp_add(dp_read("车数据")) |>
    task_item_dp_add(dp_head()) |>
    task_item_dp_add(dp_collect()) |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(10)

  task_create("cars/dp_read") |>
    task_item_dp_add(dp_read("车数据")) |>
    task_item_dp_add(dp_head(5)) |>
    task_item_dp_add(dp_collect()) |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(5)
  
  task_create("cars/dp_read") |>
    task_item_dp_add(dp_read("车数据")) |>
    task_item_dp_add(dp_tail(6)) |>
    task_item_dp_add(dp_collect()) |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(6)
  
  temp_remove()
})

test_that("<dp_slice_max/ dp_slice_min>", {
  sample_config_init()
  
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> as_tibble() |> ds_append("车数据")
  
  task_create("cars/dp_read") |>
    task_item_dp_add(dp_read("车数据")) |>
    task_item_dp_add(dp_n_max("cyl", 3)) |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(3)
  
  task_create("cars/dp_read") |>
    task_item_dp_add(dp_read("车数据")) |>
    task_item_dp_add(dp_n_min("disp", 3)) |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(3)
  
  temp_remove()
})

