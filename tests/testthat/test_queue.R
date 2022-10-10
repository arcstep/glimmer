library(dplyr, warn.conflicts = F)
library(tibble, warn.conflicts = F)

config_init(tempdir())
task_queue_init()

prepare_data <- function() {
  expand_grid(batchFolder = c("scheduel_2002-10-01", "scheduel_2002-10-02", "scheduel_2002-10-03"),
              taskFolder = c("A", "B", "C")) |>
    purrr::pwalk(function(batchFolder, taskFolder) {
      p <- get_path("IMPORT", batchFolder, taskFolder)
      fs::dir_create(p)
      fs::file_touch(c(paste0(p, "/1.csv"), paste0(p, "/2.csv")))
    })
}

clear_dir <- function() {
  get_path("IMPORT") |> remove_dir()
}

test_that("定义导入任务", {
  prepare_data()
  task_import_define("A")
  task_import_define("B")

  get_path("TASK", "__IMPORT_TASK__", "A") |> fs::dir_exists() |>
    testthat::expect_true()
  get_path("TASK", "__IMPORT_TASK__", "B") |> fs::dir_exists() |>
    testthat::expect_true()
  
  clear_dir()
})

test_that("扫描导入任务", {
  prepare_data()
  task_import_define("A")
  task_import_define("B")
  
  task_import_scan()

  clear_dir()
})