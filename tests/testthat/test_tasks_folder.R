library(dplyr)
library(tibble)

test_that("设置目标文件夹", {
  set_topic("IMPORT", "/tmp/glimmer/IMPORT")
  get_topic("IMPORT") |> expect_equal("/tmp/glimmer/IMPORT")
  get_path("IMPORT", "abc") |> as.character() |> expect_equal("/tmp/glimmer/IMPORT/abc")
})

test_that("查看目标文件夹下的脚本", {
  set_topic("IMPORT", "/tmp/glimmer/IMPORT")
  if(fs::dir_exists("/tmp/glimmer/IMPORT")) {
    fs::dir_delete(get_path("IMPORT"))
  }
  fs::dir_create(get_path("IMPORT"))
  fs::file_touch(get_path("IMPORT", "2.R"))
  fs::file_touch(get_path("IMPORT", "1.R"))
  fs::file_touch(get_path("IMPORT", "3.R"))
  get_path("IMPORT") |>
    get_task_plan() |>
    expect_equal(
      tribble(
        ~name, ~path,
        "1.R", "/tmp/glimmer/IMPORT/1.R",
        "2.R", "/tmp/glimmer/IMPORT/2.R",
        "3.R", "/tmp/glimmer/IMPORT/3.R"
      ) |> mutate(path = fs::as_fs_path(path))
    )

})

test_that("执行目标文件夹下的脚本", {
  set_topic("STATE", "/tmp/glimmer/STATE")
  set_topic("IMPORT", "/tmp/glimmer/IMPORT")
  if(fs::dir_exists("/tmp/glimmer/IMPORT")) {
    fs::dir_delete(get_path("IMPORT"))
  }
  fs::dir_create(get_path("IMPORT"))
  write("f2 <- f1 + 1", get_path("IMPORT", "2.R"))
  write("f1 <- 1", get_path("IMPORT", "1.R"))
  write("f3 <- f2 + f1", get_path("IMPORT", "3.R"))
  get_path("IMPORT") |> run_task()
  
  expect_equal(f3, 3)
})

test_that("执行目标文件夹下的脚本，哪怕是多层子目录", {
  set_topic("STATE", "/tmp/glimmer/STATE")
  set_topic("IMPORT", "/tmp/glimmer/IMPORT")
  if(fs::dir_exists("/tmp/glimmer/IMPORT")) {
    fs::dir_delete(get_path("IMPORT"))
  }
  fs::dir_create(get_path("IMPORT"))
  fs::dir_create(get_path("IMPORT", "1-FF"))
  fs::dir_create(get_path("IMPORT", "2-EE"))
  write("f2 <- f1 + 1", get_path("IMPORT", "1-FF/2.R"))
  write("f1 <- 1", get_path("IMPORT", "1-FF/1.R"))
  write("f3 <- f2 + f1", get_path("IMPORT", "2-EE/1.R"))
  get_path("IMPORT") |> run_task()
  expect_equal(f3, 3)
})