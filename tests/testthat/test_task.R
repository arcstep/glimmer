test_that("定义任务：空任务", {
  sample_init()
  
  ## 空任务
  task_create(taskName = "mytask1", force = TRUE) |>
    task_run() |>
    testthat::expect_warning("Empty")

  temp_remove()
})

test_that("定义任务：string类型", {
  sample_init()

  task_create(taskName = "A", force = TRUE)
  task_read("A")$taskType |>
    testthat::expect_equal("__UNKNOWN__")

  script_item_add(taskName = "A", script = "ls()", type = "string")
  task_read("A")$items |> nrow() |>
    testthat::expect_equal(1)
  
  script_item_add(taskName = "A", script = "ls()", globalVars = list(batchFoler = NULL), type = "string")
  task_read("A")$items |> nrow() |>
    testthat::expect_equal(2)
  
  script_item_add(taskName = "A", script = "ls()", globalVars = list(batchFoler = "schedual_1001", data = 1:3), type = "string")
  task_read("A")$items |> nrow() |>
    testthat::expect_equal(3)
  
  temp_remove()
})

test_that("定义任务：file类型，且使用管道风格", {
  sample_init()

  task_create(taskName = "B", force = TRUE) |>
    script_item_add(script = "A/a.R", type = "file") |>
    script_item_add(script = "result <- (x |> filter(age > 6))", type = "string")
  task_read("B")$items |> nrow() |>
    testthat::expect_equal(2)
  
  temp_remove()
})

test_that("定义任务：dir类型", {
  sample_init()

  task_create(taskName = "C", force = TRUE) |>
    script_item_add(script = "A", type = "dir")
  task_read("C")$items |> nrow() |>
    testthat::expect_equal(1)

  temp_remove()
})

test_that("任务脚本：赠、改、删、调序", {
  sample_init()
  
  task_create(taskName = "TaskA", force = TRUE) |>
    script_func_add(script = "ds_demo", params = list("demoDataset" = "mpg")) |>
    script_func_add(script = "ds_head") |>
    script_func_add(script = "ds_arrange", params = list("columns" = "displ"))
  task_read("TaskA")$items$script |>
    identical(c("ds_demo", "ds_head", "ds_arrange")) |>
    testthat::expect_true()

  script_item_remove("TaskA", 2)
  task_read("TaskA")$items$script |>
    identical(c("ds_demo", "ds_arrange")) |>
    testthat::expect_true()

  "TaskA" |> script_func_add(script = "ds_head")
  "TaskA" |> script_item_exchange(3, 2)
  task_read("TaskA")$items$script |>
    identical(c("ds_demo", "ds_head", "ds_arrange")) |>
    testthat::expect_true()

  "TaskA" |> script_func_update(2, script = "ds_tail", params = list(n = 3))
  task_read("TaskA")$items$script |>
    identical(c("ds_demo", "ds_tail", "ds_arrange")) |>
    testthat::expect_true()
  
  temp_remove()
})


test_that("<task_run>: withEnv", {
  sample_init()
  task_create("mytask") |>
    script_var_add() |>
    task_run(withEnv = T) |> names() |>
    testthat::expect_identical(c("result", "env"))
})

test_that("<task_run>: string|expr|empty", {
  sample_init()
  
  ## 空的执行环境中
  task_create(taskName = "A-str", force = TRUE) |>
    script_string_add(script = "ls()") |>
    task_run() |>
    testthat::expect_equal("@task")

  task_create(taskName = "A-expr", force = TRUE) |>
    script_expr_add(script = expression({ls()})) |>
    task_run() |>
    testthat::expect_equal("@task")
  
  ## 获取执行环境中的变量
  (task_create(taskName = "A-expr", force = TRUE) |>
      script_expr_add(script = expression({`@task`})) |>
    task_run())$online |>
    testthat::expect_false()

  ## 设置执行环境变量
  task_create(taskName = "A-expr", force = TRUE) |>
    script_expr_add(script = expression({myname})) |>
    task_run(myname = "xueyile") |>
    testthat::expect_equal("xueyile")
  
  task_create(taskName = "A-expr", force = TRUE) |>
      script_var_add(globalVars = list("myname" = "xueyile")) |>
      script_expr_add(script = expression({myname})) |>
      task_run() |>
    testthat::expect_equal("xueyile")
  
  ## 映射输出
  task_create(taskName = "C-expr", force = TRUE) |>
    script_var_add(globalVars = list("a" = 3, "b" = 3)) |>
    script_expr_add(script = expression({list(x = a^2, y = b*2)}),
                  outputAssign = c("m")) |>
    script_expr_add(script = expression({m$x+m$y})) |>
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
  task_create(taskName = "fun01", force = TRUE) |>
    script_func_add(script = "mycars") |>
    task_run() |>
    testthat::expect_error("No Schema Defined")

  temp_remove()
})

test_that("<task_run>: 预定义函数管道", {
  sample_init()

  ## 默认参数
  task_create(taskName = "fun01", force = TRUE) |>
    script_func_add("gali_import_cars") |>
    task_run() |> nrow() |>
    testthat::expect_equal(nrow(mtcars))
  
  ## 管道连接
  task_create(taskName = "fun02", force = TRUE) |>
    script_func_add("gali_import_cars") |>
    script_func_add("gali_ds_filter_cyl") |>
    task_run() |> nrow() |>
    testthat::expect_equal(nrow(mtcars |> filter(cyl == 4)))
  
  task_create(taskName = "fun02", force = TRUE) |>
    script_func_add("gali_import_cars") |>
    script_item_add(type = "func", script = "gali_ds_filter_cyl", params = list(i_cyl = 6)) |>
    task_run() |> nrow() |>
    testthat::expect_equal(nrow(mtcars |> filter(cyl == 6)))
  
  ## 显式使用映射参数
  task_create(taskName = "fun03", force = TRUE) |>
    script_func_add("gali_import_cars") |>
    script_func_add(script = "gali_ds_filter_cyl", params = list(i_cyl = 6)) |>
    script_string_add(script = "`@ds` |> head(5)") |>
    task_run() |> nrow() |>
    testthat::expect_equal(5)
  
  task_create(taskName = "fun03", force = TRUE) |>
    script_func_add("gali_import_cars") |>
    script_func_add(script = "gali_ds_filter_cyl", params = list(i_cyl = 6), outputAssign = "car6") |>
    script_string_add(script = "car6 |> head(5)") |>
    task_run() |> nrow() |>
    testthat::expect_equal(5)
  
  task_create(taskName = "fun03", force = TRUE) |>
    script_func_add("gali_import_cars", outputAssign = "mycars") |>
    script_string_add(script = "mycars |> head(5)", outputAssign = "mycars") |>
    script_func_add(script = "gali_ds_filter_cyl", params = list(i_cyl = 4), inputAssign = list("d" = "mycars")) |>
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
  task_create(taskName = "mytask1")
  task_create(taskName = "mytask1") |>
    testthat::expect_error("Task Already Exist")
  
  temp_remove()
})

test_that("编辑任务：正常流程", {
  sample_init()
  
  ## 进入编辑模式
  taskName <- "mytask1"
  taskName |> task_run() |>
    testthat::expect_warning("Empty Task") |>
    testthat::expect_error("No Task Define")
  
  task_create(taskName) |>
    script_func_add("ds_demo", params = list("demoDataset" = "mpg")) |>
    task_edit_snap()
  snapId <- task_read(taskName)$snapId
  is.null(snapId) |>
    testthat::expect_false()
  get_path("SNAP", "TASK_DEFINE", snapId, "main.rds") |>
    fs::file_exists() |>
    testthat::expect_true()
  
  ## 在编辑模式中追加
  taskName |> script_item_add(type = "func", script = "ds_head")
  identical("ds_demo",
            task_read(taskName)$items$script) |>
    testthat::expect_true()
  taskName |> task_run() |> nrow() |>
    testthat::expect_equal(nrow(ds_demo("mpg")))
  identical(c("ds_demo", "ds_head"),
            task_read(taskName, snap = TRUE)$items$script) |>
    testthat::expect_true()
  taskName |> task_run(snap = TRUE) |> nrow() |>
    testthat::expect_equal(10)
  
  ## 从编辑模式保存
  taskName |> task_save()
  identical(c("ds_demo", "ds_head"), task_read(taskName)$items$script) |>
    testthat::expect_true()
  taskName |> task_run() |> nrow() |>
    testthat::expect_equal(10)
  
  ## 结束编辑模式
  taskName |> script_item_add(type = "func",
                          script = "ds_arrange",
                          params = list(columns = "displ", desc = TRUE))
  taskName |> task_submit()
  identical(c("ds_demo", "ds_head", "ds_arrange"), task_read(taskName)$items$script) |>
    testthat::expect_true()
  task_run(taskName)$displ[[1]] |>
    testthat::expect_equal(3.1)
  
  temp_remove()
})

test_that("编辑任务：克隆、移除和放弃编辑", {
  sample_init()
  
  ## 克隆
  task_create("mytask1") |>
    script_func_add("ds_demo", params = list("demoDataset" = "mpg")) |>
    task_clone("mytask2")
  
  task_read("mytask2")$items |> nrow() |>
    testthat::expect_equal(1)

  ## 移除 
  task_remove("mytask1") |>
    task_read() |>
    testthat::expect_error("No Task Define")
  
  ## 放弃编辑
  task_edit_snap("mytask2") |>
    script_item_add(type = "func", script = "ds_head") |>
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
  taskName <- "mytask1"
  taskName |> task_run() |>
    testthat::expect_warning("Empty Task") |>
    testthat::expect_error("No Task Define")
  
  ## 按快照执行任务
  task_create(taskName) |>
    task_edit_snap() |>
    script_item_add(type = "func",
                  script = "ds_demo",
                  params = list("demoDataset" = "mpg")) |>
    script_item_add(type = "func",
                  script = "ds_arrange",
                  params = list(columns = "displ", desc = TRUE)) |>
    script_item_add(type = "func", script = "ds_head")
  taskName |> task_run(snap = TRUE) |> nrow() |>
    testthat::expect_equal(10)
  taskName |> task_run(snap = TRUE, stepToRun = 2) |> nrow() |>
    testthat::expect_equal(nrow(ds_demo("mpg")))
  
  ## 按定义执行任务
  task_submit(taskName)
  taskName |> task_run() |> nrow() |>
    testthat::expect_equal(10)
  taskName |> task_run(stepToRun = 2) |> nrow() |>
    testthat::expect_equal(nrow(ds_demo("mpg")))
  
  temp_remove()
})

test_that("获取任务执行参数", {
  sample_init()

  ##  
  taskName <- "mytask1"

  task_create(taskName) |>
    script_func_add("ds_demo", params = list("demoDataset" = "mpg")) |>
    script_item_add(type = "func",
                    script = "ds_head",
                    params = list(n = 2),
                    ## 注意入参映射表达：name为函数参数、value为任务参数
                    inputAssign = list("n" = "N", "d" = "@ds")) |>
    script_item_add(type = "func",
                    script = "ds_arrange",
                    params = list(columns = "displ", desc = TRUE, by_group = T),
                    inputAssign = list(desc = "desc", by_group = "mygroup"))
  task_run(taskName, N = 4, desc = T, mygroup = F)
  task_params_assign(taskName)
  
  identical(c("ds_demo", "ds_head", "ds_arrange"),
            task_read(taskName)$items$script) |>
    testthat::expect_true()

  ##
  toAssign <- task_params_assign(taskName)
  toAssign$taskParam |> identical(c("N", "desc", "mygroup")) |>
    testthat::expect_true()
  toAssign$value[[1]] |> testthat::expect_equal(2)
  toAssign$value[[2]] |> testthat::expect_true()
  
  temp_remove()
})
