test_that("导入流程", {
  sample_config_init()
  import_init()
  task_queue_init()

  ## 列举所有素材
  import_files_all() |> nrow() |>
    testthat::expect_equal(5)

  ## 扫描新素材
  import_files_scan()
  import_files_read() |> nrow() |>
    testthat::expect_equal(5)
  
  ## 匹配任务
  task_all() |> filter(taskType=="__IMPORT__")
  import_task_match()
  import_files_read() |> filter(!is.na(taskId)) |> nrow() |>
    testthat::expect_equal(1)
  
  ## 创建批处理任务
  import_task_queue_create()
  task_queue_todo(taskTypes = "__IMPORT__")
  
  ## 执行批处理任务
  import_task_queue_run()
  
  temp_remove()
})