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

test_that("<task_run>", {
  sample_config_init()
  sample_import_files()
  
  task_run("task_sample_simple") |> nrow() |>
    testthat::expect_equal(1)

  task_run("task_sample_simple", withQueue = TRUE) |> nrow() |>
    testthat::expect_equal(1)
  
  task_run("task_sample_dir") |> ncol() |>
    testthat::expect_equal(3)
  
  task_run("task_sample_define_param") |> nrow() |>
    testthat::expect_equal(4)
  
  task_run("task_sample_runtime_param", myage = 6) |> nrow() |>
    testthat::expect_equal(1)
  
  temp_remove()
})

test_that("<task_run_[string|file|dir|expr]>", {
  sample_config_init()
  sample_import_files()

  task_run_expr(expression({mtcars |> as_tibble()})) |> nrow() |>
    testthat::expect_equal(32)
  
  task_run_expr(expression({mtcars |> filter(cyl == a)}), a = 6) |> nrow() |>
    testthat::expect_equal(7)

  x <- 6
  task_run_expr(expression({mtcars |> filter(cyl == a)}), a = x) |> nrow() |>
    testthat::expect_equal(7)
  
  task_run_string("mtcars") |> nrow() |>
    testthat::expect_equal(32)
  
  task_run_file("SIMPLE/a.R") |> nrow() |>
    testthat::expect_equal(5)

  task_run_dir("SIMPLE")$id[[1]] |>
    testthat::expect_equal("liyihan-5")
  
  temp_remove()
})

test_that("使用gali函数", {
  sample_config_init()
  sample_import_files()
  
  ##
  assign(
    "gali_myfunc1",
    function() { mtcars |> as_tibble() },
    envir = globalenv())
  task_create(taskId = "Gali_01", list()) |>
    task_gali_add("gali_myfunc1")
  task_run("Gali_01") |> nrow() |>
    testthat::expect_equal(nrow(mtcars))

  ##
  assign(
    "gali_myarrange",
    function(d = NULL, sv_columns) {
      (d %empty% get(s_OUTPUT)) |>
        arrange(!!!syms(sv_columns))},
    envir = globalenv())
  task_create(taskId = "Gali_02", list()) |>
    task_gali_add("gali_myfunc1") |>
    task_gali_add("gali_myarrange", params = list(s_OUTPUT = "@result", sv_columns = "disp"))
  task_run("Gali_02")$disp[[1]] |>
    testthat::expect_equal(min(mtcars$disp))
  
  task_create(taskId = "Gali_03", list()) |>
    task_gali_add("gali_myarrange",
                       params = list("s_OUTPUT" = "x", "x" = mtcars, sv_columns = "disp"))
  task_run("Gali_03")$disp[[1]] |>
    testthat::expect_equal(min(mtcars$disp))

  task_create(taskId = "Gali_04", list("@result" = mtcars)) |>
    task_gali_add("gali_myarrange",
                  params = list(sv_columns = "disp"))
  task_run("Gali_04")$disp[[1]] |>
    testthat::expect_equal(min(mtcars$disp))
  
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
