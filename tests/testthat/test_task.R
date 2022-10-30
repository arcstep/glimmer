test_that("定义任务：空任务", {
  sample_init()
  
  ## 空任务
  task_create(taskId = "mytask1", force = TRUE) |>
    task_run() |>
    testthat::expect_warning("Empty")

  temp_remove()
})

test_that("定义任务：string类型", {
  sample_init()

  task_create(taskId = "A", force = TRUE)
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

  task_create(taskId = "B", force = TRUE) |>
    task_item_add(script = "A/a.R", type = "file") |>
    task_item_add(script = "result <- (x |> filter(age > 6))", type = "string")
  task_read("B")$items |> nrow() |>
    testthat::expect_equal(2)
  
  temp_remove()
})

test_that("定义任务：dir类型", {
  sample_init()

  task_create(taskId = "C", force = TRUE) |>
    task_item_add(script = "A", type = "dir")
  task_read("C")$items |> nrow() |>
    testthat::expect_equal(1)

  temp_remove()
})

test_that("任务脚本：赠、改、删、调序", {
  sample_init()
  
  task_create(taskId = "TaskA", force = TRUE) |>
    task_func_add(script = "ds_demo", params = list("demoDataset" = "mpg")) |>
    task_func_add(script = "ds_head") |>
    task_func_add(script = "ds_arrange", params = list("columns" = "displ"))
  task_read("TaskA")$items$script |>
    identical(c("ds_demo", "ds_head", "ds_arrange")) |>
    testthat::expect_true()

  task_item_remove("TaskA", 2)
  task_read("TaskA")$items$script |>
    identical(c("ds_demo", "ds_arrange")) |>
    testthat::expect_true()

  "TaskA" |> task_func_add(script = "ds_head")
  "TaskA" |> task_item_exchange(3, 2)
  task_read("TaskA")$items$script |>
    identical(c("ds_demo", "ds_head", "ds_arrange")) |>
    testthat::expect_true()

  "TaskA" |> task_func_update(2, script = "ds_tail", params = list(n = 3))
  task_read("TaskA")$items$script |>
    identical(c("ds_demo", "ds_tail", "ds_arrange")) |>
    testthat::expect_true()
  
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
  task_create(taskId = "A-str", force = TRUE) |>
    task_string_add(script = "ls()") |>
    task_run() |>
    testthat::expect_equal("@task")

  task_create(taskId = "A-expr", force = TRUE) |>
    task_expr_add(script = expression({ls()})) |>
    task_run() |>
    testthat::expect_equal("@task")
  
  ## 获取执行环境中的变量
  (task_create(taskId = "A-expr", force = TRUE) |>
      task_expr_add(script = expression({`@task`})) |>
    task_run())$online |>
    testthat::expect_false()

  ## 设置执行环境变量
  task_create(taskId = "A-expr", force = TRUE) |>
    task_expr_add(script = expression({myname})) |>
    task_run(myname = "xueyile") |>
    testthat::expect_equal("xueyile")
  
  task_create(taskId = "A-expr", force = TRUE) |>
      task_global_add(globalVars = list("myname" = "xueyile")) |>
      task_expr_add(script = expression({myname})) |>
      task_run() |>
    testthat::expect_equal("xueyile")
  
  ## 映射输出
  task_create(taskId = "C-expr", force = TRUE) |>
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
  task_create(taskId = "fun01", force = TRUE) |>
    task_func_add(script = "mycars") |>
    task_run() |>
    testthat::expect_error("No Schema Defined")

  temp_remove()
})

test_that("<task_run>: 预定义函数管道", {
  sample_init()

  ## 默认参数
  task_create(taskId = "fun01", force = TRUE) |>
    task_func_add("gali_import_cars") |>
    task_run() |> nrow() |>
    testthat::expect_equal(nrow(mtcars))
  
  ## 管道连接
  task_create(taskId = "fun02", force = TRUE) |>
    task_func_add("gali_import_cars") |>
    task_func_add("gali_ds_filter_cyl") |>
    task_run() |> nrow() |>
    testthat::expect_equal(nrow(mtcars |> filter(cyl == 4)))
  
  task_create(taskId = "fun02", force = TRUE) |>
    task_func_add("gali_import_cars") |>
    task_item_add(type = "func", script = "gali_ds_filter_cyl", params = list(i_cyl = 6)) |>
    task_run() |> nrow() |>
    testthat::expect_equal(nrow(mtcars |> filter(cyl == 6)))
  
  ## 显式使用映射参数
  task_create(taskId = "fun03", force = TRUE) |>
    task_func_add("gali_import_cars") |>
    task_func_add(script = "gali_ds_filter_cyl", params = list(i_cyl = 6)) |>
    task_string_add(script = "`@ds` |> head(5)") |>
    task_run() |> nrow() |>
    testthat::expect_equal(5)
  
  task_create(taskId = "fun03", force = TRUE) |>
    task_func_add("gali_import_cars") |>
    task_func_add(script = "gali_ds_filter_cyl", params = list(i_cyl = 6), outputAsign = "car6") |>
    task_string_add(script = "car6 |> head(5)") |>
    task_run() |> nrow() |>
    testthat::expect_equal(5)

  task_create(taskId = "fun03", force = TRUE) |>
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

test_that("定义任务：重复定义", {
  sample_init()
  
  ## 空任务
  task_create(taskId = "mytask1")
  task_create(taskId = "mytask1") |>
    testthat::expect_error("Task Already Exist")
  
  temp_remove()
})

test_that("编辑任务：正常流程", {
  sample_init()
  
  ## 进入编辑模式
  taskId <- "mytask1"
  taskId |> task_run() |>
    testthat::expect_warning("Empty Task") |>
    testthat::expect_error("No Task Define")
  
  task_create(taskId) |>
    task_func_add("ds_demo", params = list("demoDataset" = "mpg")) |>
    task_edit()
  snapId <- task_read(taskId)$snapId
  is.null(snapId) |>
    testthat::expect_false()
  get_path("SNAP", "TASK_DEFINE", snapId, "main.rds") |>
    fs::file_exists() |>
    testthat::expect_true()
  
  ## 在编辑模式中追加
  taskId |> task_item_add(type = "func", script = "ds_head")
  identical("ds_demo",
            task_read(taskId)$items$script) |>
    testthat::expect_true()
  taskId |> task_run() |> nrow() |>
    testthat::expect_equal(nrow(ds_demo("mpg")))
  identical(c("ds_demo", "ds_head"),
            task_read(taskId, snap = TRUE)$items$script) |>
    testthat::expect_true()
  taskId |> task_run(snap = TRUE) |> nrow() |>
    testthat::expect_equal(10)
  
  ## 从编辑模式保存
  taskId |> task_save()
  identical(c("ds_demo", "ds_head"),
            task_read(taskId)$items$script) |>
    testthat::expect_true()
  taskId |> task_run() |> nrow() |>
    testthat::expect_equal(10)
  
  ## 结束编辑模式
  taskId |> task_item_add(type = "func",
                          script = "ds_arrange",
                          params = list(columns = "displ", desc = TRUE))
  taskId |> task_submit()
  identical(c("ds_demo", "ds_head", "ds_arrange"), task_read(taskId)$items$script) |>
    testthat::expect_true()
  task_run(taskId)$displ[[1]] |>
    testthat::expect_equal(3.1)
  
  temp_remove()
})

test_that("编辑任务：克隆、移除和放弃编辑", {
  sample_init()
  
  ## 克隆
  task_create("mytask1") |>
    task_func_add("ds_demo", params = list("demoDataset" = "mpg")) |>
    task_clone("mytask2")
  
  task_read("mytask2")$items |> nrow() |>
    testthat::expect_equal(1)

  ## 移除 
  task_remove("mytask1") |>
    task_read() |>
    testthat::expect_error("No Task Define")
  
  ## 放弃编辑
  task_edit("mytask2") |>
    task_item_add(type = "func", script = "ds_head") |>
    task_run(snap = TRUE) |>
    nrow() |>
    testthat::expect_equal(10)
  task_discard("mytask2") |>
    task_run(snap = TRUE) |>
    nrow() |>
    testthat::expect_equal(nrow(ds_demo("mpg")))
  
  temp_remove()
})

test_that("<task_run>: stepToRun", {
  sample_init()
  
  ## 
  taskId <- "mytask1"
  taskId |> task_run() |>
    testthat::expect_warning("Empty Task") |>
    testthat::expect_error("No Task Define")
  
  ## 按快照执行任务
  task_create(taskId) |>
    task_edit() |>
    task_item_add(type = "func",
                  script = "ds_demo",
                  params = list("demoDataset" = "mpg")) |>
    task_item_add(type = "func",
                  script = "ds_arrange",
                  params = list(columns = "displ", desc = TRUE)) |>
    task_item_add(type = "func", script = "ds_head")
  taskId |> task_run(snap = TRUE) |> nrow() |>
    testthat::expect_equal(10)
  taskId |> task_run(snap = TRUE, stepToRun = 2) |> nrow() |>
    testthat::expect_equal(nrow(ds_demo("mpg")))
  
  ## 按定义执行任务
  task_submit(taskId)
  taskId |> task_run() |> nrow() |>
    testthat::expect_equal(10)
  taskId |> task_run(stepToRun = 2) |> nrow() |>
    testthat::expect_equal(nrow(ds_demo("mpg")))
  
  temp_remove()
})

