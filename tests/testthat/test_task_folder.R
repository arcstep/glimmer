library(dplyr)
library(tibble)

test_that("设置目标文件夹", {
  set_topic("IMPORT", "/tmp/glimmer/IMPORT")
  get_topic("IMPORT") |> expect_equal("/tmp/glimmer/IMPORT")
  get_path("IMPORT", "abc") |> as.character() |> expect_equal("/tmp/glimmer/IMPORT/abc")
})

test_that("查看目标文件夹下的脚本", {
  set_topic("IMPORT", "/tmp/glimmer/IMPORT")
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