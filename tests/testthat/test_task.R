test_that("定义任务：手工添加items", {
  sample_init()
  
  ## 空任务
  task_create(taskId = "mytask1") |>
    task_run() |>
    testthat::expect_equal(NULL)

  ## 创建任务时添加items
  task_create(taskId = "mytask2", items = tibble(
    "scriptType" = "string",
    "taskScript" = "ls()",
    "params" = list(NULL),
    "inputAsign" = list(NULL),
    "outputAsign" = list(NULL)
  )) |> task_run() |>
    testthat::expect_equal("@task")

  ## 
  task_create(taskId = "mytask2", items = tibble(
    "scriptType" = c("string", "expr"),
    "taskScript" = c("ls()", expression(x)),
    "params" = list(list("x" = 1:10), list(NULL)),
    "inputAsign" = list(NULL),
    "outputAsign" = list(NULL)
  )) |> task_run() |>
    testthat::expect_identical(1:10)

  temp_remove()
})

test_that("定义任务：string类型", {
  sample_init()

  task_create(taskId = "A")
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
  sample_init()

  task_create(taskId = "B") |>
    task_item_add(taskScript = "A/a.R", scriptType = "file") |>
    task_item_add(taskScript = "result <- (x |> filter(age > 6))", scriptType = "string")
  task_read("B")$items |> nrow() |>
    testthat::expect_equal(2)
  
  temp_remove()
})

test_that("定义任务：dir类型", {
  sample_init()

  task_create(taskId = "C") |>
    task_item_add(taskScript = "A", scriptType = "dir")
  task_read("C")$items |> nrow() |>
    testthat::expect_equal(1)

  temp_remove()
})

test_that("<task_run>: withEnv", {
  sample_init()
  task_create("mytask") |> task_run(withEnv = T) |> names()
})

test_that("<task_run>: string|expr|empty", {
  sample_init()
  
  ## 空的执行环境中
  task_create(taskId = "A-str") |>
    task_item_add(taskScript = "ls()", scriptType = "string") |>
    task_run() |>
    testthat::expect_equal("@task")

  task_create(taskId = "A-expr") |>
    task_item_add(taskScript = expression({ls()}), scriptType = "expr") |>
    task_run() |>
    testthat::expect_equal("@task")
  
  ## 获取执行环境中的变量
  (task_create(taskId = "A-expr") |>
    task_item_add(taskScript = expression({`@task`}), scriptType = "expr") |>
    task_run())$online |>
    testthat::expect_false()

  ## 设置执行环境变量
  task_create(taskId = "A-expr") |>
    task_item_add(scriptType = "expr", taskScript = expression({myname})) |>
    task_run(myname = "xueyile") |>
    testthat::expect_equal("xueyile")
  
  task_create(taskId = "A-expr") |>
      task_item_add(scriptType = "empty", params = list("myname" = "xueyile")) |>
      task_item_add(scriptType = "expr", taskScript = expression({myname})) |>
      task_run() |>
    testthat::expect_equal("xueyile")
  
  ## 映射输出
  task_create(taskId = "C-expr") |>
    task_item_add(scriptType = "empty",
                  params = list("a" = 3, "b" = 3)) |>
    task_item_add(scriptType = "expr",
                  taskScript = expression({list(x = a^2, y = b*2)}),
                  outputAsign = c("m")) |>
    task_item_add(scriptType = "expr",
                  taskScript = expression({m$x+m$y})) |>
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

test_that("<task_run>: function", {
  sample_init()
  ## 无参数
  mycars <- function() { mtcars |> as_tibble() }
  mycyl <- function(i_cyl = 4) { mtcars |> as_tibble() |> filter(cyl == i_cyl) }
  myarrange <- function(d, sv_columns) {
    d |> arrange(!!!syms(sv_columns))
  }
  
  ## 默认参数
  task_create(taskId = "fun01") |>
    task_item_add(scriptType = "function", taskScript = "mycars") |>
    task_run() |> nrow() |>
    testthat::expect_equal(nrow(mtcars))

  ## 设置参数
  task_create(taskId = "fun02") |>
    task_item_add(scriptType = "function", taskScript = "mycyl") |>
    task_run() |> nrow() |>
    testthat::expect_equal(nrow(mtcars |> filter(cyl == 4)))
  
  task_create(taskId = "fun02") |>
    task_item_add(scriptType = "function", taskScript = "mycyl", params = list(i_cyl = 6)) |>
    task_run() |> nrow() |>
    testthat::expect_equal(nrow(mtcars |> filter(cyl == 6)))
  
  temp_remove()
  
})

test_that("<task_run>: gali", {
  sample_init()
  
  ##
  assign(
    "gali_myfunc1",
    function() { mtcars |> as_tibble() },
    envir = globalenv())
  task_create(taskId = "Gali_01") |>
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
  task_create(taskId = "Gali_02") |>
    task_gali_add("gali_myfunc1") |>
    task_gali_add("gali_myarrange", params = list(s_OUTPUT = "@result", sv_columns = "disp"))
  task_run("Gali_02")$disp[[1]] |>
    testthat::expect_equal(min(mtcars$disp))
  
  task_create(taskId = "Gali_03") |>
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
