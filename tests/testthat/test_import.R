test_that("导入流程", {
  sample_config_init()
  import_init()
  task_queue_init()
  sample_dataset_init()

  ## 列举所有素材
  import_files_all() |> nrow() |>
    testthat::expect_equal(5)

  ## 扫描新素材
  import_files_scan()
  import_dataset_read() |> collect() |> nrow() |>
    testthat::expect_equal(5)
  
  ## 匹配任务
  task_all() |> filter(taskType=="__IMPORT__")
  import_dataset_task_match()
  import_dataset_read() |> filter(!is.na(taskId)) |> collect() |> nrow() |>
    testthat::expect_equal(2)
  
  ## 创建批处理任务
  import_task_queue_create()
  is.na(task_queue_todo(taskTypes = "__IMPORT__")$runAt) |>
    testthat::expect_equal(c(T, T))
  
  ## 执行批处理任务
  import_task_queue_run()
  ds_read0("student") |> nrow() |>
    testthat::expect_equal(3)
  
  temp_remove()
})