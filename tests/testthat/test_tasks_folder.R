library(dplyr)
library(tibble)

set_topic("STATE", "/tmp/glimmer/STATE")
if(fs::dir_exists("/tmp/glimmer/STATE")) {
  fs::dir_delete(get_path("STATE"))
}

set_topic("IMPORT", "/tmp/glimmer/IMPORT")
if(fs::dir_exists("/tmp/glimmer/IMPORT")) {
  fs::dir_delete(get_path("IMPORT"))
}

set_topic("CACHE", "/tmp/glimmer/CACHE")
if(fs::dir_exists("/tmp/glimmer/CACHE")) {
  fs::dir_delete(get_path("CACHE"))
}

set_topic("TASK/BUILD", "/tmp/glimmer/TASK/BUILD")
if(fs::dir_exists("/tmp/glimmer/TASK/BUILD")) {
  fs::dir_delete(get_path("TASK/BUILD"))
}

set_topic("TASK/IMPORT", "/tmp/glimmer/TASK/IMPORT")
if(fs::dir_exists("/tmp/glimmer/TASK/IMPORT")) {
  fs::dir_delete(get_path("TASK/IMPORT"))
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
  get_task_scripts("IMPORT") |>
    testthat::expect_equal(
      tribble(
        ~name, ~path,
        "1.R", "/tmp/glimmer/IMPORT/1.R",
        "2.R", "/tmp/glimmer/IMPORT/2.R",
        "3.R", "/tmp/glimmer/IMPORT/3.R"
      ) |> mutate(path = fs::as_fs_path(path))
    )
  fs::dir_delete(get_path("IMPORT"))
})

test_that("执行目标文件夹下的脚本", {
  fs::dir_create(get_path("TASK/BUILD"))
  write("f2 <- f1 + 1", get_path("TASK/BUILD", "2.R"))
  write("f1 <- 1", get_path("TASK/BUILD", "1.R"))
  write("f3 <- f2 + f1", get_path("TASK/BUILD", "3.R"))
  run_task_scripts("TASK/BUILD")

  expect_equal(f3, 3)
  fs::dir_delete(get_path("TASK/BUILD"))
})

test_that("执行目标文件夹下的脚本，哪怕是多层子目录", {
  fs::dir_create(get_path("TASK/IMPORT", "1-FF"))
  fs::dir_create(get_path("TASK/IMPORT", "2-EE"))
  write("f2 <- f1 + 1", get_path("TASK/IMPORT", "1-FF/2.R"))
  write("f1 <- 1", get_path("TASK/IMPORT", "1-FF/1.R"))
  write("f3 <- f2 + f1", get_path("TASK/IMPORT", "2-EE/1.R"))
  run_task_scripts("TASK/IMPORT")
  expect_equal(f3, 3)
  fs::dir_delete(get_path("TASK/IMPORT"))
})

test_that("根据导入文件夹，执行导入计划", {
  fs::dir_create(get_path("IMPORT", "task001/车型"))
  d <- mtcars |> as_tibble() |> slice(1:10)
  d |> readr::write_excel_csv(get_path("IMPORT", "task001", "车型", "1.csv"))
  
  fs::dir_create(get_path("TASK/IMPORT"))
  '
  path <- get_path("IMPORT", get_topic("__DOING_TASK_FOLDER__"), "车型")
  if(path |> fs::dir_exists()) {
    arrow::open_dataset(path, format = "csv") |> collect() |> write_dataset("车型")
  }
  ' |> write(get_path("TASK/IMPORT", "1.R"))

  taskfolder_todo()
  testthat::expect_equal(nrow(d), nrow(read_dataset("车型")))

  fs::dir_delete(get_path("STATE"))
  fs::dir_delete(get_path("CACHE"))
  fs::dir_delete(get_path("IMPORT"))
  fs::dir_delete(get_path("TASK/IMPORT"))
})

