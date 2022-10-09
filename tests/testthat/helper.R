library(tibble, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(glimmer, warn.conflicts = FALSE)

rootPath <- tempdir()

sample_task_define <- function() {
  task_create(taskId = "build_cars", taskType = "__BUILD__") |>
    task_item_add(taskScript = "BUILD/cars.R", scriptType = "file")

  task_create(taskId = "task_sample_simple") |>
    task_item_add(taskScript = "SIMPLE/a.R", scriptType = "file") |>
    task_item_add(taskScript = "result <- (x |> filter(age > 6))", scriptType = "string")

  task_create(taskId = "task_sample_dir") |>
    task_item_add(taskScript = "SIMPLE", scriptType = "dir")

  task_create(taskId = "task_sample_define_param") |>
    task_item_add(taskScript = "SIMPLE/a.R", scriptType = "file") |>
    task_item_add(taskScript = "result <- (x |> filter(age > myage))", params = list(myage = 3), scriptType = "string")

  task_create(taskId = "task_sample_runtime_param") |>
    task_item_add(taskScript = "SIMPLE/a.R", scriptType = "file") |>
    task_item_add(taskScript = "result <- (x |> filter(age > myage))", scriptType = "string")

  task_create(taskId = "task_sample_error") |>
    task_item_add(taskScript = "stop('I m an error!')", scriptType = "string")

  task_create(taskId = "task_sample_file_not_exist") |>
    task_item_add(taskScript = "NOT_EXISTING_FILE", scriptType = "file")

  task_create(taskId = "task_sample_dir_not_exist") |>
    task_item_add(taskScript = "NOT_EXISTING_DIR", scriptType = "dir")

  task_create(taskId = "task_sample_empty_dir") |>
    task_item_add(taskScript = "EMPTY_FOLDER", scriptType = "dir")
}

temp_config_init <- function() {
  config_init(rootPath)
}

sample_config_init <- function() {
  config_init(rootPath, option = list(
    "IMPORT" = testthat::test_path("_sample_import") |> fs::path_abs(),
    "TASK_DEFINE" = testthat::test_path("_sample_task_define") |> fs::path_abs(),
    "TASK_SCRIPTS" = testthat::test_path("_sample_task_scripts") |> fs::path_abs()
  ))
}

temp_remove <- function() rootPath |> fs::dir_delete()
