library(dplyr)
library(tibble)

test_that("读写状态文件", {
  set_topic("STATE", "/tmp/glimmer/STATE")

  d <- tibble(taskFolder = c("task_2022_08_08"))
  state_write("IMPORT", d)
  (state_read("IMPORT") |> collect())$taskFolder |>
    expect_equal("task_2022_08_08")
  
  get_path("STATE") |> remove_dir()
})

