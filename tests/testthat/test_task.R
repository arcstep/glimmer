test_that("定义任务：空任务", {
  sample_init()
  
  ## 空任务
  task_create(taskId = "mytask1") |>
    task_run() |>
    testthat::expect_warning("Empty")

  temp_remove()
})

test_that("定义任务：string类型", {
  sample_init()

  task_create(taskId = "A")
  task_read("A")$taskType |>
    testthat::expect_equal("__UNKNOWN__")

  task_item_add(taskId = "A", script = "ls()", type = "string")
  task_read("A")$items |> nrow() |>
    testthat::expect_equal(1)
  
  task_item_add(taskId = "A", script = "ls()", globalVars = list(batchFoler = NULL), type = "string")
  task_read("A")$items |> nrow() |>
    testthat::expect_equal(2)
  
  task_item_add(taskId = "A", script = "ls()", globalVars = list(batchFoler = "schedual_1001", data = 1:3), type = "string")
  task_read("A")$items |> nrow() |>
    testthat::expect_equal(3)
  
  temp_remove()
})

test_that("定义任务：file类型，且使用管道风格", {
  sample_init()

  task_create(taskId = "B") |>
    task_item_add(script = "A/a.R", type = "file") |>
    task_item_add(script = "result <- (x |> filter(age > 6))", type = "string")
  task_read("B")$items |> nrow() |>
    testthat::expect_equal(2)
  
  temp_remove()
})

test_that("定义任务：dir类型", {
  sample_init()

  task_create(taskId = "C") |>
    task_item_add(script = "A", type = "dir")
  task_read("C")$items |> nrow() |>
    testthat::expect_equal(1)

  temp_remove()
})

test_that("<task_run>: withEnv", {
  sample_init()
  task_create("mytask") |>
    task_global_add() |>
    task_run(withEnv = T) |> names() |>
    testthat::expect_identical(c("result", "env"))
})

test_that("<task_run>: string|expr|empty", {
  sample_init()
  
  ## 空的执行环境中
  task_create(taskId = "A-str") |>
    task_string_add(script = "ls()") |>
    task_run() |>
    testthat::expect_equal("@task")

  task_create(taskId = "A-expr") |>
    task_expr_add(script = expression({ls()})) |>
    task_run() |>
    testthat::expect_equal("@task")
  
  ## 获取执行环境中的变量
  (task_create(taskId = "A-expr") |>
      task_expr_add(script = expression({`@task`})) |>
    task_run())$online |>
    testthat::expect_false()

  ## 设置执行环境变量
  task_create(taskId = "A-expr") |>
    task_expr_add(script = expression({myname})) |>
    task_run(myname = "xueyile") |>
    testthat::expect_equal("xueyile")
  
  task_create(taskId = "A-expr") |>
      task_global_add(globalVars = list("myname" = "xueyile")) |>
      task_expr_add(script = expression({myname})) |>
      task_run() |>
    testthat::expect_equal("xueyile")
  
  ## 映射输出
  task_create(taskId = "C-expr") |>
    task_global_add(globalVars = list("a" = 3, "b" = 3)) |>
    task_expr_add(script = expression({list(x = a^2, y = b*2)}),
                  outputAsign = c("m")) |>
    task_expr_add(script = expression({m$x+m$y})) |>
    task_run() |>
    testthat::expect_equal(3^2+3*2)

  temp_remove()
})


test_that("<task_run>: file|dir", {
  sample_init()

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

test_that("<task_run>: 没有预定义", {
  sample_init()

  ## 默认参数
  task_create(taskId = "fun01") |>
    task_func_add(script = "mycars") |>
    task_run() |>
    testthat::expect_error("No Schema Defined")

  temp_remove()
})

test_that("<task_run>: 预定义函数管道", {
  sample_init()

  ## 默认参数
  task_create(taskId = "fun01") |>
    task_func_add("gali_import_cars") |>
    task_run() |> nrow() |>
    testthat::expect_equal(nrow(mtcars))
  
  ## 管道连接
  task_create(taskId = "fun02") |>
    task_func_add("gali_import_cars") |>
    task_func_add("gali_ds_filter_cyl") |>
    task_run() |> nrow() |>
    testthat::expect_equal(nrow(mtcars |> filter(cyl == 4)))
  
  task_create(taskId = "fun02") |>
    task_func_add("gali_import_cars") |>
    task_item_add(type = "func", script = "gali_ds_filter_cyl", params = list(i_cyl = 6)) |>
    task_run() |> nrow() |>
    testthat::expect_equal(nrow(mtcars |> filter(cyl == 6)))
  
  ## 显式使用映射参数
  task_create(taskId = "fun03") |>
    task_func_add("gali_import_cars") |>
    task_func_add(script = "gali_ds_filter_cyl", params = list(i_cyl = 6)) |>
    task_string_add(script = "`@ds` |> head(5)") |>
    task_run() |> nrow() |>
    testthat::expect_equal(5)
  
  task_create(taskId = "fun03") |>
    task_func_add("gali_import_cars") |>
    task_func_add(script = "gali_ds_filter_cyl", params = list(i_cyl = 6), outputAsign = "car6") |>
    task_string_add(script = "car6 |> head(5)") |>
    task_run() |> nrow() |>
    testthat::expect_equal(5)

  task_create(taskId = "fun03") |>
    task_func_add("gali_import_cars", outputAsign = "mycars") |>
    task_string_add(script = "mycars |> head(5)", inputAsign = "mycars", outputAsign = "mycars") |>
    task_func_add(script = "gali_ds_filter_cyl", params = list(i_cyl = 4), inputAsign = list("d" = "mycars")) |>
    task_run() |> nrow() |>
    testthat::expect_equal(1)
  
  temp_remove()
})

test_that("运行任务：异常情况", {
  sample_init()

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
