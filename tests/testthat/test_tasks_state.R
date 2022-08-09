library(dplyr)
library(tibble)

test_that("读写状态文件", {
  set_topic("STATE", "/tmp/glimmer/STATE")
  fs::dir_delete(get_path("STATE"))
  write_state("IMPORT", "task_2022_08_08")
  (read_state("IMPORT", "task_2022_08_08") |> collect())$title |>
    expect_equal("task_2022_08_08")
})

