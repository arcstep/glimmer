library(dplyr)
library(tibble)

set_topic("STATE", "/tmp/glimmer/STATE")
set_topic("IMPORT", "/tmp/glimmer/IMPORT")
set_topic("CACHE", "/tmp/glimmer/CACHE")
set_topic("TASK/BUILD", "/tmp/glimmer/TASK/BUILD")
set_topic("TASK/IMPORT", "/tmp/glimmer/TASK/IMPORT")

clear_dir <- function() {
  get_path("STATE") |> remove_dir()
  get_path("IMPORT") |> remove_dir()
  get_path("CACHE") |> remove_dir()
  get_path("TASK/BUILD") |> remove_dir()
  get_path("TASK/IMPORT") |> remove_dir()
}

test_that("设置目标文件夹", {
  set_topic("IMPORT", "/tmp/glimmer/IMPORT")
  get_topic("IMPORT") |> testthat::expect_equal("/tmp/glimmer/IMPORT")
  get_path("IMPORT", "abc") |> as.character() |> testthat::expect_equal("/tmp/glimmer/IMPORT/abc")
})

test_that("查看目标文件夹下的脚本", {
  fs::dir_create(get_path("IMPORT"))
  fs::file_touch(get_path("IMPORT", "2.R"))
  fs::file_touch(get_path("IMPORT", "1.R"))
  fs::file_touch(get_path("IMPORT", "3.R"))
  task_read("IMPORT") |>
    testthat::expect_equal(
      tribble(
        ~name, ~path,
        "1.R", "/tmp/glimmer/IMPORT/1.R",
        "2.R", "/tmp/glimmer/IMPORT/2.R",
        "3.R", "/tmp/glimmer/IMPORT/3.R"
      ) |> mutate(path = fs::as_fs_path(path))
    )
  clear_dir()
})

test_that("执行目标文件夹下的脚本", {
  fs::dir_create(get_path("TASK/BUILD"))
  write("f2 <- f1 + 1", get_path("TASK/BUILD", "2.R"))
  write("f1 <- 1", get_path("TASK/BUILD", "1.R"))
  write("f3 <- f2 + f1", get_path("TASK/BUILD", "3.R"))
  task_run(taskScript = "TASK/BUILD")
  expect_equal(f3, 3)
  clear_dir()
})

test_that("执行目标文件夹下的脚本，哪怕是多层子目录", {
  fs::dir_create(get_path("TASK/BUILD", "1-FF"))
  fs::dir_create(get_path("TASK/BUILD", "2-EE"))
  write("f2 <- f1 + 1", get_path("TASK/BUILD", "1-FF/2.R"))
  write("f1 <- 1", get_path("TASK/BUILD", "1-FF/1.R"))
  write("f3 <- f2 + f1", get_path("TASK/BUILD", "2-EE/1.R"))
  task_run(taskScript = "TASK/BUILD")
  expect_equal(f3, 3)
  clear_dir()
})

#
prepare_csv <- function(range, taskFolder, dsName) {
  fs::dir_create(get_path("IMPORT", taskFolder, dsName))
  tibble(a = 1:100, b = 1:100) |>
    as_tibble() |>
    slice(range) |>
    readr::write_excel_csv(get_path("IMPORT", taskFolder, dsName, "1.csv"))
}

test_that("根据导入文件夹，执行导入计划", {
  prepare_csv( 1:10, taskFolder = "task001", dsName = "车型")
  prepare_csv(11:20, taskFolder = "task002", dsName = "车型")
  prepare_csv(21:30, taskFolder = "task003", dsName = "车型")
  
  fs::dir_create(get_path("TASK/IMPORT"))
  '
  path <- get_path("IMPORT", get_topic("__DOING_TASK_FOLDER__"), "车型")
  if(path |> fs::dir_exists()) {
    arrow::open_dataset(path, format = "csv") |> collect() |> ds_write("车型")
  }
  ' |> write(get_path("TASK/IMPORT", "1.R"))

  import_todo()
  testthat::expect_equal(nrow(ds_read("车型")), 30)
  clear_dir()
})

test_that("仅对未处理文件夹执行导入计划", {
  prepare_csv( 1:10, taskFolder = "task001", dsName = "车型")
  prepare_csv(11:20, taskFolder = "task002", dsName = "车型")
  
  fs::dir_create(get_path("TASK/IMPORT"))
  '
  path <- get_path("IMPORT", get_topic("__DOING_TASK_FOLDER__"), "车型")
  if(path |> fs::dir_exists()) {
    arrow::open_dataset(path, format = "csv") |> collect() |> ds_write("车型")
  }
  ' |> write(get_path("TASK/IMPORT", "1.R"))
  
  import_todo(taskScript = "TASK/IMPORT")
  testthat::expect_equal(nrow(ds_read("车型")), 20)
  
  prepare_csv(21:30, taskFolder = "task003", dsName = "车型")
  import_todo(taskScript = "TASK/IMPORT")
  testthat::expect_equal(nrow(ds_read("车型")), 30)
  clear_dir()
})


test_that("手工指定导入文件夹，执行导入脚本", {
  prepare_csv(1:2, taskFolder = "task001", dsName = "车型")
  prepare_csv(3:4, taskFolder = "task002", dsName = "车型")
  prepare_csv(5:6, taskFolder = "task003", dsName = "车型")
  prepare_csv(7:8, taskFolder = "task001", dsName = "cars")
  prepare_csv(9:10, taskFolder = "task002", dsName = "cars")

  create_dir(get_path("TASK/IMPORT"))
  '
  path <- get_path("IMPORT", get_topic("__DOING_TASK_FOLDER__"), "车型")
  if(path |> fs::dir_exists()) {
    arrow::open_dataset(path, format = "csv") |> collect() |> ds_write("车型")
  }
  ' |> write(get_path("TASK/IMPORT", "1.R"))
  '
  path <- get_path("IMPORT", get_topic("__DOING_TASK_FOLDER__"), "cars")
  if(path |> fs::dir_exists()) {
    arrow::open_dataset(path, format = "csv") |> collect() |> ds_write("cars")
  }
  ' |> write(get_path("TASK/IMPORT", "2.R"))

  import_redo(todo = c("task002", "task003"), taskScript = "TASK/IMPORT")
  testthat::expect_equal(nrow(ds_read("车型")), 4)
  testthat::expect_equal(nrow(ds_read("cars")), 2)
  
  clear_dir()
})
