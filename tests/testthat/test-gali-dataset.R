test_that("<gali_read_dataset / gali_dataset_collect>", {
  sample_config_init()
  
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> slice(1:10) |> as_tibble() |> ds_append("车数据")
  gali_read_dataset("车数据") |>
    task_run_gali(cacheTopic = "CACHE") |> collect() |> nrow() |>
    testthat::expect_equal(10)
  
  task_create("cars/gali_read_dataset") |>
    task_item_gali_add(gali_read_dataset("车数据")) |>
    task_item_gali_add(gali_dataset_collect()) |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(10)
  
  temp_remove()
})

test_that("<gali_dataset_filter>：一般流程", {
  sample_config_init()
  sample_import_files()

  ## 使用默认的 @result 获得返回值
  gali_dataset_filter("cyl", ">", 6) |>
    task_run_gali(`@result` = mtcars) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl > 6) |> nrow())

  ## 使用新的变量名
  gali_dataset_filter("cyl", ">", 6, s_dataName = "mydata") |>
    task_run_gali(`mydata` = mtcars) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl > 6) |> nrow())
  
  temp_remove()
})

test_that("<gali_dataset_filter>：比较操作符", {
  gali_dataset_filter("cyl", "<", 6) |>
    task_run_gali(`@result` = mtcars) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl < 6) |> nrow())
  
  gali_dataset_filter("cyl", ">=", 6) |>
    task_run_gali(`@result` = mtcars) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl >= 6) |> nrow())
  
  gali_dataset_filter("cyl", "<=", 6) |>
    task_run_gali(`@result` = mtcars) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl <= 6) |> nrow())

  gali_dataset_filter("cyl", "==", 6) |>
    task_run_gali(`@result` = mtcars) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl == 6) |> nrow())
  
  gali_dataset_filter("cyl", "!=", 6) |>
    task_run_gali(`@result` = mtcars) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl != 6) |> nrow())
})

test_that("<gali_dataset_filter>：正则表达式", {
  gali_dataset_filter("rowname", "%regex%", "Mazda") |>
    task_run_gali(`@result` = mtcars |> rownames_to_column()) |> nrow() |>
    testthat::expect_equal(
      mtcars |>
        rownames_to_column() |>
        filter(stringr::str_detect(rowname, "Mazda")) |>
        nrow())

  gali_dataset_filter("rowname", "%not-regex%", "Mazda") |>
    task_run_gali(`@result` = mtcars |> rownames_to_column()) |> nrow() |>
    testthat::expect_equal(
      mtcars |>
        rownames_to_column() |>
        filter(stringr::str_detect(rowname, "Mazda", negate = T)) |>
        nrow())
})

test_that("<gali_dataset_filter>：包含", {
  gali_dataset_filter("rowname", "%in%", c("Honda Civic", "Fiat 128")) |>
    task_run_gali(`@result` = mtcars |> rownames_to_column()) |> nrow() |>
    testthat::expect_equal(
      mtcars |>
        rownames_to_column() |>
        filter(rowname %in% c("Honda Civic", "Fiat 128")) |>
        nrow())

  gali_dataset_filter("rowname", "%nin%", c("Honda Civic", "Fiat 128")) |>
    task_run_gali(`@result` = mtcars |> rownames_to_column()) |> nrow() |>
    testthat::expect_equal(
      mtcars |>
        rownames_to_column() |>
        filter(!(rowname %in% c("Honda Civic", "Fiat 128"))) |>
        nrow())
  
})

test_that("<gali_dataset_filter>：比较时间和日期", {
  d <- tibble(
    n = 1:5,
    day = c("2020-05-01", "2020-06-01", "2020-05-11", "2020-07-3", "2019-12-30") |> lubridate::as_date(),
    dt = c("2020-05-01 11:11:11", "2020-06-01 11:11:11", "2020-05-11 11:11:11", "2020-07-3 11:11:11", "2019-12-30 11:11:11") |> lubridate::as_datetime(tz = "Asia/Shanghai")
  )
  
  ## 测试日期格式比较
  gali_dataset_filter("day", "date# >", "2020-05-30") |> task_run_gali(`@result` = d) |> nrow() |>
    testthat::expect_equal(
      d |> filter(day > as_date("2020-05-30")) |> nrow()
    )
  
  ## 测试时间格式比较
  gali_dataset_filter("dt", "time# >", "2020-05-30") |> task_run_gali(`@result` = d) |> nrow() |>
    testthat::expect_equal(
      d |> filter(dt > as_datetime("2020-05-30")) |> nrow()
    )
})

test_that("<gali_dataset_head/ gali_dataset_tail>", {
  sample_config_init()
  
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> as_tibble() |> ds_append("车数据")

  task_create("cars/gali_read_dataset") |>
    task_item_gali_add(gali_read_dataset("车数据")) |>
    task_item_gali_add(gali_dataset_head()) |>
    task_item_gali_add(gali_dataset_collect()) |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(10)

  task_create("cars/gali_read_dataset") |>
    task_item_gali_add(gali_read_dataset("车数据")) |>
    task_item_gali_add(gali_dataset_head(5)) |>
    task_item_gali_add(gali_dataset_collect()) |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(5)
  
  task_create("cars/gali_read_dataset") |>
    task_item_gali_add(gali_read_dataset("车数据")) |>
    task_item_gali_add(gali_dataset_tail(6)) |>
    task_item_gali_add(gali_dataset_collect()) |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(6)
  
  temp_remove()
})

test_that("<gali_dataset_n_max/ gali_dataset_n_min>", {
  sample_config_init()
  
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> as_tibble() |> ds_append("车数据")
  
  task_create("cars/gali_read_dataset") |>
    task_item_gali_add(gali_read_dataset("车数据")) |>
    task_item_gali_add(gali_dataset_n_max("cyl", 3)) |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(3)
  
  task_create("cars/gali_read_dataset") |>
    task_item_gali_add(gali_read_dataset("车数据")) |>
    task_item_gali_add(gali_dataset_n_min("disp", 3)) |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(3)
  
  temp_remove()
})

test_that("<gali_dataset_select>", {
  sample_config_init()
  
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> as_tibble() |> ds_append("车数据")
  
  task_create("cars/gali_read_dataset") |>
    task_item_gali_add(gali_read_dataset("车数据")) |>
    task_item_gali_add(gali_dataset_select("cyl")) |>
    task_item_gali_add(gali_dataset_collect()) |>
    task_run() |>
    ncol() |>
    testthat::expect_equal(1)
  
  task_create("cars/gali_read_dataset") |>
    task_item_gali_add(gali_read_dataset("车数据")) |>
    task_item_gali_add(gali_dataset_select(c("cyl", "disp"))) |>
    task_item_gali_add(gali_dataset_collect()) |>
    task_run() |>
    ncol() |>
    testthat::expect_equal(2)
  
  task_create("cars/gali_read_dataset") |>
    task_item_gali_add(gali_read_dataset("车数据")) |>
    task_item_gali_add(gali_dataset_select(c("cyl", "disp"), b_everything = T)) |>
    task_item_gali_add(gali_dataset_collect()) |>
    task_run() |>
    ncol() |>
    testthat::expect_equal(17)

  task_create("cars/gali_read_dataset") |>
    task_item_gali_add(gali_read_dataset("车数据")) |>
    task_item_gali_add(gali_dataset_select(c("cyl", "disp"), s_regex = "^@")) |>
    task_item_gali_add(gali_dataset_collect()) |>
    task_run() |>
    ncol() |>
    testthat::expect_equal(7)
  
  temp_remove()
})


test_that("<gali_arrange>", {
  sample_config_init()
  
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> as_tibble() |> ds_append("车数据")
  
  (task_create("cars/gali_read_dataset") |>
    task_item_gali_add(gali_read_dataset("车数据")) |>
    task_item_gali_add(gali_dataset_arrange("disp")) |>
    task_item_gali_add(gali_dataset_collect()) |>
    task_run() |>
    head(1))$disp |>
    testthat::expect_equal(min(mtcars$disp))
  
  (task_create("cars/gali_read_dataset") |>
      task_item_gali_add(gali_read_dataset("车数据")) |>
      task_item_gali_add(gali_dataset_arrange("disp", b_desc = T)) |>
      task_item_gali_add(gali_dataset_collect()) |>
      task_run() |>
      head(1))$disp |>
    testthat::expect_equal(max(mtcars$disp))
  
  temp_remove()
})

test_that("<gali_rename>", {
  sample_config_init()
  
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> as_tibble() |> ds_append("车数据")
  
  task_create("cars/gali_read_dataset") |>
    task_item_gali_add(gali_read_dataset("车数据")) |>
    task_item_gali_add(gali_dataset_rename("MY_DISP", "disp")) |>
    task_item_gali_add(gali_dataset_rename("中国队", "cyl")) |>
    task_item_gali_add(gali_dataset_select(c("中国队", "MY_DISP"))) |>
    task_item_gali_add(gali_dataset_collect()) |>
    task_run() |>
    ncol() |>
    testthat::expect_equal(2)

  temp_remove()
})
