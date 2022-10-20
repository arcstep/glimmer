test_that("简单的datatable", {
  mtcars |> DT_table() |>
    testthat::expect_snapshot()
})
