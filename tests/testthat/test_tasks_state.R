library(dplyr)
library(tibble)

test_that("读写状态文件", {
  set_topic("STATE", "/tmp/glimmer/STATE")
  fs::dir_delete(get_path("STATE"))
  d <- tibble(taskFolder = c("task_2022_08_08"))
  write_state("IMPORT", d)
  (read_state("IMPORT") |> collect())$taskFolder |>
    expect_equal("task_2022_08_08")
})

