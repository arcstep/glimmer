library(dplyr, warn.conflicts = F)
library(tibble, warn.conflicts = F)

config_init(tempdir())

clear_dir <- function() {
  get_path("TASK_DEFINE") |> remove_dir()
}

test_that("定义任务", {
  task_create(taskId = "A", list())
  task_read("A")$taskType |>
    testthat::expect_equal("__UNKNOWN__")

  task_add(taskId = "A", taskScript = "ls()", scriptType = "string")
  task_read("A")$items |> nrow() |>
    testthat::expect_equal(1)
  
  task_add(taskId = "A", taskScript = "ls()", params = list(batchFoler = NULL), scriptType = "string")
  task_read("A")$items |> nrow() |>
    testthat::expect_equal(2)
  
  task_add(taskId = "A", taskScript = "ls()", params = list(batchFoler = "schedual_1001", data = 1:3), scriptType = "string")
  task_read("A")$items |> nrow() |>
    testthat::expect_equal(3)
  
  clear_dir()
})

test_that("定义导入任务：执行字符串脚本", {
  task_create(taskId = "B", list())
  task_add(
    taskId = "B",
    taskScript = "resp <- tibble::tibble('x'=1:4)",
    scriptType = "string")
  task_add(
    taskId = "B",
    taskScript = "resp |> mutate(y=2*x)",
    scriptType = "string")
  task_run("B")
  
  clear_dir()
})

test_that("定义导入任务：执行文件", {
  task_create(taskId = "B", list())
  task_add(
    taskId = "B",
    taskScript = "tests/testthat/tasks/a.R",
    scriptType = "file")
  task_add(
    taskId = "B",
    taskScript = "result <- (x |> filter(age > 6))",
    scriptType = "string")
  task_run("B")
  
  clear_dir()
})

test_that("定义导入任务：定义任务时带参数", {
  task_create(taskId = "B", list())
  task_add(
    taskId = "B",
    taskScript = "tests/testthat/tasks/a.R",
    scriptType = "file")
  task_add(
    taskId = "B",
    taskScript = "result <- (x |> filter(age > myage))",
    params = list(myage = 3),
    scriptType = "string")
  task_run("B") |> nrow() |> testthat::expect_equal(4)
  
  clear_dir()
})

test_that("定义导入任务：运行任务时带参数", {
  task_create(taskId = "B", list())
  task_add(
    taskId = "B",
    taskScript = "tests/testthat/tasks/a.R",
    scriptType = "file")
  task_add(
    taskId = "B",
    taskScript = "result <- (x |> filter(age > myage))",
    scriptType = "string")
  task_run("B", myage = 6) |> nrow() |> testthat::expect_equal(1)
  
  clear_dir()
})
