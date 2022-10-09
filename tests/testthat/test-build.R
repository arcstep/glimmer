test_that("multiplication works", {
  sample_config_init()
  ds_init(
    "mycars",
    data = mtcars |> as_tibble() |> tibble::rowid_to_column("id"), keyColumns = "id")

  task_run("build_cars")
  ds_read("mycars") |>
    collect() |>
    nrow() |>
    testthat::expect_equal(nrow(mtcars))
  
  temp_remove()
})
