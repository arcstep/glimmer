test_that("简单的datatable", {
  mtcars |> gali_DT() |>
    testthat::expect_snapshot()
})
