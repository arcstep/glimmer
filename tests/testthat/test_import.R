test_that("<import_changed> 扫描新素材变化", {
  sample_init()
  import_changed() |> nrow() |>
    testthat::expect_equal(5)
  
  get_path("IMPORT") |>
    fileSnapshot(md5sum = TRUE, recursive = F) |>
    saveRDS(get_path("SNAP", "import.rds"))
  
  folders <- readRDS(get_path("SNAP", "import.rds")) |>
    changedFiles()
  c(folders$added, folders$changed) |> length() |>
    testthat::expect_equal(0)
  
  ## 在原批次文件夹中新增
  get_path("IMPORT", "schedual_101", "cars") |> fs::dir_create()
  mtcars |> as_tibble() |> slice(1:5) |>
    readr::write_excel_csv(get_path("IMPORT", "schedual_101", "cars/1_10.csv"))

  resp <- import_changed()
  resp$batchFolder |> testthat::expect_equal("schedual_101")
  resp$filePath |> testthat::expect_equal("cars/1_10.csv")

  ## 在原批次文件夹中修改
  get_path("IMPORT", "schedual_01", "cars") |> fs::dir_create()
  mtcars |> as_tibble() |> slice(1:5) |>
    readr::write_excel_csv(get_path("IMPORT", "schedual_01", "cars/1_10.csv"))

  resp <- import_changed()
  ("schedual_101" %in% resp$batchFolder) |> testthat::expect_true()
  ("schedual_01" %in% resp$batchFolder) |> testthat::expect_true()
  resp$filePath |> length() |> testthat::expect_equal(6)
  
  temp_remove()
})

test_that("<import_scan> 过滤要导入的文件", {
  sample_init()
  import_scan()
  import_search() |> nrow() |>
    testthat::expect_equal(5)

  ## 按批次查询
  import_search(batchMatch = "01$") |> nrow() |>
    testthat::expect_equal(4)
  
  ## 按文件查询
  import_search("stu") |> nrow() |>
    testthat::expect_equal(1)
  import_search("sco") |> nrow() |>
    testthat::expect_equal(2)
  
  ## 按批次和文件名综合查询
  import_search("1.csv", batchMatch = "01") |> nrow() |>
    testthat::expect_equal(1)

  temp_remove()
})

test_that("<import_run> scrore/student", {
  sample_init()
  import_scan()
  files <- import_search()
  files |> nrow() |>
    testthat::expect_equal(5)

  ## 执行任务
  ds_read("score") |> collect() |> nrow() |>
    testthat::expect_equal(0)
  ##
  import_csv(import_search("score.csv$"), dsName = "score")
  ds_read("score") |> collect() |> nrow() |>
    testthat::expect_equal(5)

  ##  
  import_csv(import_search("student.csv$"), dsName = "student")
  ds_read("student") |> collect() |> nrow() |>
    testthat::expect_equal(3)
  
  temp_remove()
})

test_that("<import_run> 导入多个批次中的csv文件", {
  sample_init()
  import_scan()
  
  import_search("^A/1") |>
    import_csv_preview(keyColumns = c("name")) |>
    nrow() |>
    testthat::expect_equal(6)

  import_search("^A/1") |>
    import_csv(dsName = "A/1", keyColumns = c("name")) |>
    import_todo_flag()
  ds_read("A/1") |> collect() |> nrow() |>
    testthat::expect_equal(6)

  temp_remove()
})


test_that("<import_run> 自定义导入文件", {
  sample_init()
  import_scan()
  
  # import_search("^A/1") |> import_files_preview(function(path) {
  #   arrow::read_csv_arrow(path) |> as_tibble() |> slice(-1)
  # })
  import_search("^A/1", todoFlag = c(T, F)) |>
    import_files(dsName = "A/1", keyColumns = c("name"), func = function(path) {
      arrow::read_csv_arrow(path) |> slice(-1)
    }) |>
    import_todo_flag()
  ds_read("A/1") |> collect() |> nrow() |>
    testthat::expect_equal(6)
  
  temp_remove()
})