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
  task_read("A")
  
  task_add(taskId = "A", taskScript = "ls()", params = list(batchFoler = NULL), scriptType = "string")
  task_read("A")
  
  task_add(taskId = "A", taskScript = "ls()", params = list(batchFoler = "schedual_1001", data = 1:3), scriptType = "string")
  task_read("A")
  clear_dir()
})

test_that("定义导入任务：执行字符串脚本", {
  task_create(taskId = "B", list())
  task_add(
    taskId = "B",
    taskScript = "resp <- tibble::as_tibble('x'=x)",
    params = list(x = 1:5),
    scriptType = "string")
  task_add(
    taskId = "B",
    taskScript = "y <- resp |> mutate(y=2*x)",
    scriptType = "string")
  task_run("B", "x" = c(1:10))
  
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
