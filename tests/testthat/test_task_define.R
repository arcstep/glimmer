library(dplyr, warn.conflicts = F)
library(tibble, warn.conflicts = F)

config_init(tempdir())

## dir
path <- tempdir()
path_a <- paste0(path, "/A/a.R")
path_b <- paste0(path, "/A/b.R")
path_c <- paste0(path, "/A/c.R")
path_A <- paste0(path, "/A")
path_B <- paste0(path, "/B")
path_C <- paste0(path, "/C")
fs::dir_create(paste0(path, "/A"))
fs::dir_create(paste0(path, "/B"))
## a.R
'
library(tibble)
library(tidyr)
library(dplyr)
x <- tibble(age = c(5,6,5,3,8), name = c("liyihan", "xueyile", "wangzixin", "chenzile", "adi"))
x |> arrange(desc(age))
' |> write(paste0(path, "/A/a.R"))
## b.R
'x |> mutate(id = paste0(name, "-", age))' |> write(paste0(path, "/A/b.R"))

## clear
clear_dir <- function() {
  get_path("TASK_DEFINE") |> remove_dir()
}
clear_dir()

test_that("定义任务：string类型", {
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
  
  clear_dir()
})

test_that("定义任务：file类型", {
  task_create(taskId = "B", list())
  task_item_add(
    taskId = "B",
    taskScript = path_a,
    scriptType = "file")
  task_item_add(
    taskId = "B",
    taskScript = "result <- (x |> filter(age > 6))",
    scriptType = "string")
  task_read("B")$items |> nrow() |>
    testthat::expect_equal(2)
  
  clear_dir()
})

test_that("定义任务：dir类型", {
  task_create(taskId = "C", list())
  task_item_add(
    taskId = "C",
    taskScript = path_A,
    scriptType = "dir")
  task_read("C")$items |> nrow() |>
    testthat::expect_equal(1)

  clear_dir()
})

test_that("运行任务：string", {
  task_create(taskId = "B", list())
  task_item_add(
    taskId = "B",
    taskScript = path_a,
    scriptType = "file")
  task_item_add(
    taskId = "B",
    taskScript = "result <- (x |> filter(age > 6))",
    scriptType = "string")
  task_run("B") |>
    nrow() |>
    testthat::expect_equal(1)
  
  clear_dir()
})

test_that("运行任务：dir类型", {
  task_create(taskId = "C", list())
  task_item_add(
    taskId = "C",
    taskScript = path_A,
    scriptType = "dir")
  task_run("C") |>
    ncol() |>
    testthat::expect_equal(3)
  
  clear_dir()
})

test_that("运行任务：定义时带参数", {
  task_create(taskId = "B", list())
  task_item_add(
    taskId = "B",
    taskScript = path_a,
    scriptType = "file")
  task_item_add(
    taskId = "B",
    taskScript = "result <- (x |> filter(age > myage))",
    params = list(myage = 3),
    scriptType = "string")
  task_run("B") |> nrow() |> testthat::expect_equal(4)
  
  clear_dir()
})

test_that("运行任务：运行时带参数", {
  task_create(taskId = "B", list())
  task_item_add(
    taskId = "B",
    taskScript = path_a,
    scriptType = "file")
  task_item_add(
    taskId = "B",
    taskScript = "result <- (x |> filter(age > myage))",
    scriptType = "string")
  task_run("B", myage = 6) |> nrow() |> testthat::expect_equal(1)
  
  clear_dir()
})

test_that("运行任务：抛出异常", {
  task_create(taskId = "B", list())
  task_item_add(
    taskId = "B",
    taskScript = "stop('I m an error!')",
    scriptType = "string")

  task_run("B", myage = 6) |>
    testthat::expect_error("I m an error")
  
  clear_dir()
})

test_that("运行任务：文件不存在", {
  task_create(taskId = "B", list())
  task_item_add(
    taskId = "B",
    taskScript = path_c,
    scriptType = "file")

  task_run("B", myage = 6) |>
    testthat::expect_error("No such script file")
  
  clear_dir()
})

test_that("运行任务：目录不存在", {
  task_create(taskId = "B", list())
  task_item_add(
    taskId = "B",
    taskScript = path_C,
    scriptType = "dir")
  
  task_run("B", myage = 6) |>
    testthat::expect_error("No such script dir")
  
  clear_dir()
})

test_that("运行任务：（子进程）目录不存在", {
  task_create(taskId = "B", list())
  task_item_add(
    taskId = "B",
    taskScript = path_C,
    scriptType = "dir")
  
  task_run("B", runMode = "r") |>
    testthat::expect_error("No such script dir")

  clear_dir()
})

fs::dir_delete(path)