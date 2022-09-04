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

#
prepare_csv <- function(range, taskFolder, dsName) {
  fs::dir_create(get_path("IMPORT", taskFolder, dsName))
  tibble(a = 1:100, b = 1:100) |>
    as_tibble() |>
    slice(range) |>
    readr::write_excel_csv(get_path("IMPORT", taskFolder, dsName, "1.csv"))
}

test_that("设置目标文件夹", {
  set_topic("IMPORT", "/tmp/glimmer/IMPORT")
  get_topic("IMPORT") |> testthat::expect_equal("/tmp/glimmer/IMPORT")
  get_path("IMPORT", "abc") |> as.character() |> testthat::expect_equal("/tmp/glimmer/IMPORT/abc")
})

test_that("加载YAML配置文件：一般设置", {
  fs::dir_create("/tmp/glimmer/")
  list("IMPORT" = "/tmp/glimmer/IMPORT", "BUILD" = "/tmp/glimmer/BUILD") |>
    yaml::write_yaml("/tmp/glimmer/config.yml")

  load_config("/tmp/glimmer/config.yml")
  get_topic("IMPORT") |> as.character() |>
    testthat::expect_equal("/tmp/glimmer/IMPORT")
  get_topic("BUILD") |> as.character() |>
    testthat::expect_equal("/tmp/glimmer/BUILD")
  remove_dir("/tmp/glimmer")
})

test_that("加载YAML配置文件：使用全局变量", {
  fs::dir_create("/tmp/glimmer/")
  list(
    "ROOT_PATH" = "/tmp/glimmer",
    "__MY_LOVE__" = "ABC",
    "IMPORT" = "./IMPORT",
    "BUILD" = "./BUILD") |>
    yaml::write_yaml("/tmp/glimmer/config.yml")
  
  load_config("/tmp/glimmer/config.yml")
  get_topic("IMPORT") |> as.character() |>
    testthat::expect_equal("/tmp/glimmer/IMPORT")
  get_topic("BUILD") |> as.character() |>
    testthat::expect_equal("/tmp/glimmer/BUILD")
  remove_dir("/tmp/glimmer")
})

test_that("任务文件夹尚未建立", {
  set_topic("TASK/IMPORT", "/tmp/glimmer/TASK/IMPORT")
  task_dir(taskTopic = "TASK/IMPORT") |> testthat::expect_equal(tibble())
  task_files(taskTopic = "TASK/IMPORT") |> testthat::expect_equal(tibble())
  clear_dir()
})

test_that("任务文件夹内文件为空", {
  set_topic("TASK/IMPORT", "/tmp/glimmer/TASK/IMPORT")
  create_dir(get_path("TASK/IMPORT"))
  task_dir(taskTopic = "TASK/IMPORT") |> testthat::expect_equal(tibble())
  task_files(taskTopic = "TASK/IMPORT") |> testthat::expect_equal(tibble())
  clear_dir()
})

test_that("查看脚本目录", {
  fs::dir_create(get_path("IMPORT", "ABC"))
  fs::dir_create(get_path("IMPORT", "DEF"))
  fs::file_touch(get_path("IMPORT", "DEF/2.R"))
  fs::file_touch(get_path("IMPORT", "ABC/1.R"))
  fs::file_touch(get_path("IMPORT", "ABC/4.R"))
  fs::file_touch(get_path("IMPORT", "3.R"))
  task_dir("IMPORT") |>
    testthat::expect_equal(
      tribble(
        ~topic, ~folder, ~task, ~folder_path, ~n,
        "IMPORT", "", "ABC", "/tmp/glimmer/IMPORT/ABC", 2,
        "IMPORT", "", "DEF", "/tmp/glimmer/IMPORT/DEF", 1,
        "IMPORT", "", ".", "/tmp/glimmer/IMPORT", 1
      ) |> arrange(folder_path)
    )
  clear_dir()
})

test_that("查看脚本目录：指定文件夹", {
  fs::dir_create(get_path("IMPORT", "A/BC"))
  fs::dir_create(get_path("IMPORT", "D/EF"))
  fs::file_touch(get_path("IMPORT", "D/EF/2.R"))
  fs::file_touch(get_path("IMPORT", "A/BC/1.R"))
  fs::file_touch(get_path("IMPORT", "A/BC/4.R"))
  fs::file_touch(get_path("IMPORT", "3.R"))
  task_dir("IMPORT", taskFolder = "A") |>
    testthat::expect_equal(
      tribble(
        ~topic, ~folder, ~task, ~folder_path, ~n,
        "IMPORT", "A", "BC", "/tmp/glimmer/IMPORT/A/BC", 2
      ) |> arrange(folder_path)
    )
  clear_dir()
})

test_that("查看脚本文件", {
  fs::dir_create(get_path("IMPORT", "ABC"))
  fs::file_touch(get_path("IMPORT", "2.R"))
  fs::file_touch(get_path("IMPORT", "ABC/1.R"))
  fs::file_touch(get_path("IMPORT", "3.R"))
  task_files("IMPORT") |>
    testthat::expect_equal(
      tribble(
        ~topic, ~folder, ~name, ~path,
        "IMPORT", "", "ABC/1.R", "/tmp/glimmer/IMPORT/ABC/1.R",
        "IMPORT", "", "2.R", "/tmp/glimmer/IMPORT/2.R",
        "IMPORT", "", "3.R", "/tmp/glimmer/IMPORT/3.R"
      ) |> arrange(path) |> mutate(path = fs::as_fs_path(path))
    )
  clear_dir()
})

test_that("查看脚本文件：指定文件夹", {
  fs::dir_create(get_path("IMPORT", "A/BC"))
  fs::dir_create(get_path("IMPORT", "D/EF"))
  fs::file_touch(get_path("IMPORT", "D/EF/2.R"))
  fs::file_touch(get_path("IMPORT", "A/BC/1.R"))
  fs::file_touch(get_path("IMPORT", "A/BC/4.R"))
  fs::file_touch(get_path("IMPORT", "3.R"))
  task_files("IMPORT", taskFolder = "A") |>
    testthat::expect_equal(
      tribble(
        ~topic, ~folder, ~name, ~path,
        "IMPORT", "A", "BC/1.R", "/tmp/glimmer/IMPORT/A/BC/1.R",
        "IMPORT", "A", "BC/4.R", "/tmp/glimmer/IMPORT/A/BC/4.R"
      ) |> arrange(path) |> mutate(path = fs::as_fs_path(path))
    )
  clear_dir()
})

test_that("执行目标文件夹下的脚本", {
  fs::dir_create(get_path("TASK/BUILD"))
  write("f2 <- f1 + 1", get_path("TASK/BUILD", "2.R"))
  write("f1 <- 1", get_path("TASK/BUILD", "1.R"))
  write("f3 <- f2 + f1", get_path("TASK/BUILD", "3.R"))
  task_run(taskTopic = "TASK/BUILD")
  testthat::expect_equal(f3, 3)
  clear_dir()
})

test_that("执行目标文件夹下的脚本，哪怕是多层子目录", {
  fs::dir_create(get_path("TASK/BUILD", "1-FF"))
  fs::dir_create(get_path("TASK/BUILD", "2-EE"))
  write("f2 <- f1 + 1", get_path("TASK/BUILD", "1-FF/2.R"))
  write("f1 <- 1", get_path("TASK/BUILD", "1-FF/1.R"))
  write("f3 <- f2 + f1", get_path("TASK/BUILD", "2-EE/1.R"))
  task_run(taskTopic = "TASK/BUILD")
  testthat::expect_equal(f3, 3)
  clear_dir()
})

test_that("执行特定脚本文件", {
  fs::dir_create(get_path("TASK/BUILD", "1-FF"))
  fs::dir_create(get_path("TASK/BUILD", "2-EE"))
  write("f2 <- f1 + 1", get_path("TASK/BUILD", "1-FF/2.R"))
  write("f1 <- 1", get_path("TASK/BUILD", "1-FF/1.R"))
  write("f3 <- f2 + f1", get_path("TASK/BUILD", "2-EE/1.R"))
  task_run(taskTopic = "TASK/BUILD", taskFolder = "1-FF", glob = "**/1.R")
  task_run(taskTopic = "TASK/BUILD", taskFolder = "1-FF", glob = "**/2.R")
  task_run(taskTopic = "TASK/BUILD", taskFolder = "2-EE", glob = "**/1.R")
  testthat::expect_equal(f3, 3)
  clear_dir()
})

test_that("根据导入文件夹，执行导入计划", {
  prepare_csv( 1:10, taskFolder = "task001", dsName = "车型")
  prepare_csv(11:20, taskFolder = "task002", dsName = "车型")
  prepare_csv(21:30, taskFolder = "task003", dsName = "车型")
  
  fs::dir_create(get_path("TASK/IMPORT"))
  '
  path <- get_path("IMPORT", get_importing_folder(), "车型")
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
  path <- get_path("IMPORT", get_importing_folder(), "车型")
  if(path |> fs::dir_exists()) {
    arrow::open_dataset(path, format = "csv") |> collect() |> ds_write("车型")
  }
  ' |> write(get_path("TASK/IMPORT", "1.R"))
  
  import_todo(taskTopic = "TASK/IMPORT")
  testthat::expect_equal(nrow(ds_read("车型")), 20)
  
  prepare_csv(21:30, taskFolder = "task003", dsName = "车型")
  import_todo(taskTopic = "TASK/IMPORT")
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
  path <- get_path("IMPORT", get_importing_folder(), "车型")
  if(path |> fs::dir_exists()) {
    arrow::open_dataset(path, format = "csv") |> collect() |> ds_write("车型")
  }
  ' |> write(get_path("TASK/IMPORT", "1.R"))
  '
  path <- get_path("IMPORT", get_importing_folder(), "cars")
  if(path |> fs::dir_exists()) {
    arrow::open_dataset(path, format = "csv") |> collect() |> ds_write("cars")
  }
  ' |> write(get_path("TASK/IMPORT", "2.R"))

  import_redo(todo = c("task002", "task003"), taskTopic = "TASK/IMPORT")
  testthat::expect_equal(nrow(ds_read("车型")), 4)
  testthat::expect_equal(nrow(ds_read("cars")), 2)
  
  clear_dir()
})
