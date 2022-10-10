test_that("简单的datatable", {
  mtcars |> task_DT() |>
    testthat::expect_snapshot()
})
