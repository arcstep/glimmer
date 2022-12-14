library(tibble, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(glimmer, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)

rootPath <- tempdir()

sample_dataset_init <- function() {
  config_init(rootPath)

  ##
  ds_init("student", data = tibble("name" = "adi", "age" = 5L))
  ds_init("score", schema = list(
    list(fieldName = "name", fieldType = "string"),
    list(fieldName = "english", fieldType = "int"),
    list(fieldName = "chinese", fieldType = "int"),
    list(fieldName = "math", fieldType = "int")))
}

sample_task_define <- function() {
  config_init(rootPath, option = list(
    "IMPORT" = fs::path_join(c(testthat::test_path(), "_IMPORT")),
    "TASK_DEFINE" = fs::path_join(c(testthat::test_path(), "_TASK_DEFINE")),
    "TASK_SCRIPTS" = fs::path_join(c(testthat::test_path(), "_TASK_SCRIPTS"))
  ))
  ## simple
  task_create(taskName = "build_cars", taskType = "__BUILD__") |>
    script_item_add(script = "BUILD/cars.R", type = "file", touchFiles = F)

  ## tast test
  task_create(taskName = "task_sample_simple") |>
    script_item_add(script = "SIMPLE/a.R", type = "file", touchFiles = F) |>
    script_item_add(script = "result <- (x |> filter(age > 6))", type = "string", touchFiles = F)

  task_create(taskName = "task_sample_dir") |>
    script_item_add(script = "SIMPLE", type = "dir", touchFiles = F)

  task_create(taskName = "task_sample_define_param") |>
    script_item_add(script = "SIMPLE/a.R", type = "file", touchFiles = F) |>
    script_item_add(script = "result <- (x |> filter(age > myage))", globalVars = list(myage = 3), type = "string", touchFiles = F)

  task_create(taskName = "task_sample_runtime_param") |>
    script_item_add(script = "SIMPLE/a.R", type = "file", touchFiles = F) |>
    script_item_add(script = "result <- (x |> filter(age > myage))", type = "string", touchFiles = F)

  task_create(taskName = "task_sample_error") |>
    script_item_add(script = "stop('I m an error!')", type = "string", touchFiles = F)

  ## ???????????????
  task_create(taskName = "task_sample_file_not_exist") |>
    script_item_add(script = "NOT_EXISTING_FILE", type = "file", touchFiles = F)

  ## ???????????????
  task_create(taskName = "task_sample_dir_not_exist") |>
    script_item_add(script = "NOT_EXISTING_DIR", type = "dir", touchFiles = F)

  ## ????????????
  task_create(taskName = "task_sample_empty_dir") |>
    script_item_add(script = "EMPTY_FOLDER", type = "dir", touchFiles = F)

  ## import test
  mytask_add <- function(taskName, scriptFile) {
    task_create(taskName = taskName, online = TRUE, taskType = "__IMPORT__") |>
      script_item_add(script = scriptFile, type = "file", touchFiles = F)
  }
  mytask_add("A/student", "IMPORT/student.R")
  mytask_add("A/score", "IMPORT/score.R")
}

sample_files_prepare <- function() {
  fs::dir_copy(testthat::test_path("_IMPORT"), get_path("IMPORT"), overwrite = TRUE)
  fs::dir_copy(testthat::test_path("_TASK_DEFINE"), get_path("TASK_DEFINE"), overwrite = TRUE)
  fs::dir_copy(testthat::test_path("_TASK_SCRIPTS"), get_path("TASK_SCRIPTS"), overwrite = TRUE)
  fs::dir_copy(testthat::test_path("_FUNS_DEFINE"), get_path("FUNS_DEFINE"), overwrite = TRUE)
  config_load(rootPath)
}

sample_init <- function() {
  temp_remove()
  
  config_init(rootPath)
  import_init()
  risk_data_init()
  task_queue_init()
  sample_dataset_init()
  
  sample_files_prepare()
}

temp_remove <- function() {
  if(fs::dir_exists(rootPath)) rootPath |> fs::dir_delete()
}
