test_that("<ds_collect>", {
  sample_init()
  
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> slice(1:10) |> as_tibble() |> ds_write("车数据")
  gali_read("车数据") |> collect() |> nrow() |>
    testthat::expect_equal(10)
  
  task_create("cars/gali_read") |>
    task_gali_add("gali_read", params = list(e_dsName = "车数据")) |>
    task_gali_add("gali_ds_collect") |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(10)
  
  temp_remove()
})

test_that("<gali_write>", {
  sample_init()
  
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  task_create("cars/gali_read") |>
    task_gali_add("gali_write", params = list(e_dsName = "车数据")) |>
    task_run(`@ds` = m |> slice(1:10))
  ds_read0("车数据") |> collect() |> nrow() |>
    testthat::expect_equal(10)
  
  task_create("cars/gali_read2") |>
    task_empty_add(params = list(`@ds` = m |> slice(5:15))) |>
    task_gali_add("gali_write", params = list(e_dsName = "车数据")) |>
    task_run()
  ds_read0("车数据") |> collect() |> nrow() |>
    testthat::expect_equal(15)
  
  temp_remove()
})

test_that("<gali_ds_filter>：一般流程", {
  sample_init()
  
  ## 使用默认的 @result 获得返回值
  mtcars |> gali_ds_filter("cyl", ">", 6) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl > 6) |> nrow())
  
  ## 使用新的变量名
  mtcars |> gali_ds_filter("cyl", ">", 6) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl > 6) |> nrow())
  
  temp_remove()
})

test_that("<gali_ds_filter>：比较操作符", {
  mtcars |> gali_ds_filter("cyl", "<", 6) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl < 6) |> nrow())
  
  mtcars |> gali_ds_filter("cyl", ">=", 6) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl >= 6) |> nrow())
  
  mtcars |> gali_ds_filter("cyl", "<=", 6) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl <= 6) |> nrow())
  
  mtcars |> gali_ds_filter("cyl", "==", 6) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl == 6) |> nrow())
  
  mtcars |> gali_ds_filter("cyl", "!=", 6) |> nrow() |>
    testthat::expect_equal(mtcars |> filter(cyl != 6) |> nrow())
})

test_that("<gali_ds_filter>：正则表达式", {
  mtcars |> rownames_to_column() |>
    gali_ds_filter("rowname", "%regex%", "Mazda") |>
    nrow() |>
    testthat::expect_equal(
      mtcars |>
        rownames_to_column() |>
        filter(stringr::str_detect(rowname, "Mazda")) |>
        nrow())
  
  mtcars |> rownames_to_column() |>
    gali_ds_filter("rowname", "%not-regex%", "Mazda") |>
    nrow() |>
    testthat::expect_equal(
      mtcars |>
        rownames_to_column() |>
        filter(stringr::str_detect(rowname, "Mazda", negate = T)) |>
        nrow())
})

test_that("<gali_ds_filter>：包含", {
  mtcars |> rownames_to_column() |>
    gali_ds_filter("rowname", "%in%", c("Honda Civic", "Fiat 128")) |>
    nrow() |>
    testthat::expect_equal(
      mtcars |>
        rownames_to_column() |>
        filter(rowname %in% c("Honda Civic", "Fiat 128")) |>
        nrow())
  
  mtcars |> rownames_to_column() |>
    gali_ds_filter("rowname", "%nin%", c("Honda Civic", "Fiat 128")) |>
    nrow() |>
    testthat::expect_equal(
      mtcars |>
        rownames_to_column() |>
        filter(!(rowname %in% c("Honda Civic", "Fiat 128"))) |>
        nrow())
  
})

test_that("<gali_ds_filter>：比较时间和日期", {
  d <- tibble(
    n = 1:5,
    day = c("2020-05-01", "2020-06-01", "2020-05-11", "2020-07-3", "2019-12-30") |> lubridate::as_date(),
    dt = c("2020-05-01 11:11:11", "2020-06-01 11:11:11", "2020-05-11 11:11:11", "2020-07-3 11:11:11", "2019-12-30 11:11:11") |> lubridate::as_datetime(tz = "Asia/Shanghai")
  )
  
  ## 测试日期格式比较
  d |> gali_ds_filter("day", "date# >", "2020-05-30") |> nrow() |>
    testthat::expect_equal(
      d |> filter(day > as_date("2020-05-30")) |> nrow()
    )
  
  ## 测试时间格式比较
  d |> gali_ds_filter("dt", "time# >", "2020-05-30") |> nrow() |>
    testthat::expect_equal(
      d |> filter(dt > as_datetime("2020-05-30")) |> nrow()
    )
})

test_that("<gali_ds_head/ gali_ds_tail>", {
  sample_init()
  
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> as_tibble() |> ds_write("车数据")
  
  task_create("cars/gali_read") |>
    task_gali_add("gali_read", list(e_dsName = "车数据")) |>
    task_gali_add("gali_ds_head") |>
    task_gali_add("gali_ds_collect") |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(10)
  
  task_create("cars/gali_read") |>
    task_gali_add("gali_read", list(e_dsName = "车数据")) |>
    task_gali_add("gali_ds_head", list(i_n = 5)) |>
    task_gali_add("gali_ds_collect") |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(5)
  
  task_create("cars/gali_read") |>
    task_gali_add("gali_read", list(e_dsName = "车数据")) |>
    task_gali_add("gali_ds_tail", list(i_n = 6)) |>
    task_gali_add("gali_ds_collect") |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(6)
  
  temp_remove()
})

test_that("<gali_ds_n_max/ gali_ds_n_min>", {
  sample_init()
  
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> as_tibble() |> ds_write("车数据")
  
  task_create("cars/gali_read") |>
    task_gali_add("gali_read", list(e_dsName = "车数据")) |>
    task_gali_add("gali_ds_n_max", list(s_orderColumn = "cyl", i_n = 3)) |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(3)
  
  task_create("cars/gali_read") |>
    task_gali_add("gali_read", list(e_dsName = "车数据")) |>
    task_gali_add("gali_ds_n_min", list(s_orderColumn = "disp", i_n = 3)) |>
    task_run() |>
    nrow() |>
    testthat::expect_equal(3)
  
  temp_remove()
})

test_that("<gali_ds_select>", {
  sample_init()
  
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> as_tibble() |> ds_write("车数据")
  
  task_create("cars/gali_read") |>
    task_gali_add("gali_read", list(e_dsName = "车数据")) |>
    task_gali_add("gali_ds_select", list(sv_columns = "cyl")) |>
    task_gali_add("gali_ds_collect") |>
    task_run() |>
    ncol() |>
    testthat::expect_equal(1)
  
  task_create("cars/gali_read") |>
    task_gali_add("gali_read", list(e_dsName = "车数据")) |>
    task_gali_add("gali_ds_select", list(sv_columns = c("cyl", "disp"))) |>
    task_gali_add("gali_ds_collect") |>
    task_run() |>
    ncol() |>
    testthat::expect_equal(2)
  
  task_create("cars/gali_read") |>
    task_gali_add("gali_read", list(e_dsName = "车数据")) |>
    task_gali_add("gali_ds_select", list(sv_columns = c("cyl", "disp"), b_everything = T)) |>
    task_gali_add("gali_ds_collect") |>
    task_run() |>
    ncol() |>
    testthat::expect_equal(17)
  
  task_create("cars/gali_read") |>
    task_gali_add("gali_read", list(e_dsName = "车数据")) |>
    task_gali_add("gali_ds_select", list(sv_columns = c("cyl", "disp"), s_regex = "^@")) |>
    task_gali_add("gali_ds_collect") |>
    task_run() |>
    ncol() |>
    testthat::expect_equal(7)
  
  temp_remove()
})


test_that("<gali_arrange>", {
  sample_init()
  
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> as_tibble() |> ds_write("车数据")
  
  (task_create("cars/gali_read") |>
      task_gali_add("gali_read", list(e_dsName = "车数据")) |>
      task_gali_add("gali_ds_arrange", list(sv_columns = "disp")) |>
      task_gali_add("gali_ds_collect") |>
      task_run() |>
      head(1))$disp |>
    testthat::expect_equal(min(mtcars$disp))
  
  (task_create("cars/gali_read") |>
      task_gali_add("gali_read", list(e_dsName = "车数据")) |>
      task_gali_add("gali_ds_arrange", list(sv_columns = "disp", b_desc = T)) |>
      task_gali_add("gali_ds_collect") |>
      task_run() |>
      head(1))$disp |>
    testthat::expect_equal(max(mtcars$disp))
  
  temp_remove()
})

test_that("<gali_rename>", {
  sample_init()
  
  m <- mtcars |> as_tibble() |> rownames_to_column()
  # m |> gali_ds_rename(s_newName = "中国队", s_oldName = "disp")
  
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> as_tibble() |> ds_write("车数据")
  
  resp <- task_create("cars/gali_read") |>
    task_gali_add("gali_read", list(e_dsName = "车数据")) |>
    task_gali_add("gali_ds_rename", list(s_newName = "MY_DISP", s_oldName = "disp")) |>
    task_gali_add("gali_ds_rename", list(s_newName = "中国队", s_oldName = "cyl")) |>
    task_gali_add("gali_ds_collect") |>
    task_run() |>
    names()
  ("MY_DISP" %in% resp) |> testthat::expect_true()
  ("中国队" %in% resp) |> testthat::expect_true()
  
  temp_remove()
})
