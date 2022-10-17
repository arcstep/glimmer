test_that("定义任务：string类型", {
  sample_config_init()
  sample_import_files()
  
  task_create(taskId = "A", list())
  task_read("A")$taskType |>
    testthat::expect_equal("__UNKNOWN__")

  task_item_add(taskId = "A", taskScript = "ls()", scriptType = "string")
  task_read("A")$items |> nrow() |>
    testthat::expect_equal(1)
  
  task_item_add(taskId = "A", taskScript = "ls()", params = list(batchFoler = NULL), scriptType = "string")
  task_read("A")$items |> nrow() |>
    testthat::expect_equal(2)
  
  task_item_add(taskId = "A", taskScript = "ls()", params = list(batchFoler = "schedual_1001", data = 1:3), scriptType = "string")
  task_read("A")$items |> nrow() |>
    testthat::expect_equal(3)
  
  temp_remove()
})

test_that("定义任务：file类型，且使用管道风格", {
  sample_config_init()
  sample_import_files()
  
  task_create(taskId = "B", list()) |>
    task_item_add(taskScript = "A/a.R", scriptType = "file") |>
    task_item_add(taskScript = "result <- (x |> filter(age > 6))", scriptType = "string")
  task_read("B")$items |> nrow() |>
    testthat::expect_equal(2)
  
  temp_remove()
})

test_that("定义任务：dir类型", {
  sample_config_init()
  sample_import_files()
  
  task_create(taskId = "C") |>
    task_item_add(taskScript = "A", scriptType = "dir")
  task_read("C")$items |> nrow() |>
    testthat::expect_equal(1)

  temp_remove()
})

test_that("运行任务：成功运行", {
  sample_config_init()
  sample_import_files()
  
  task_run("task_sample_simple") |> nrow() |>
    testthat::expect_equal(1)

  task_run("task_sample_dir") |> ncol() |>
    testthat::expect_equal(3)
  
  task_run("task_sample_define_param") |> nrow() |>
    testthat::expect_equal(4)
  
  task_run("task_sample_runtime_param", myage = 6) |> nrow() |>
    testthat::expect_equal(1)
  
  temp_remove()
})

test_that("运行任务：异常情况", {
  sample_config_init()
  sample_import_files()
  
  task_run("task_sample_error", myage = 6) |>
    testthat::expect_error("I m an error")
  
  task_run("task_sample_file_not_exist", myage = 6) |>
    testthat::expect_error("No such script file")
  
  task_run("task_sample_dir_not_exist", myage = 6) |>
    testthat::expect_error("No such script dir")
  
  task_run("task_sample_empty_dir", myage = 6) |>
    testthat::expect_error("None R file existing in scripts dir")
  
  temp_remove()
})
