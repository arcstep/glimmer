test_that("饼图", {
  mtcars |>
    task_plotly_pie(value = "cyl") |>
    testthat::expect_snapshot()
  
  mtcars |> count(cyl) |>
   task_plotly_pie2(value = "n", pull = c(0.1, 0, 0)) |>
    testthat::expect_snapshot()
})

test_that("柱图", {
  mtcars |>
    count(cyl) |>
    task_plotly_bar(x = "cyl", y = "n") |>
    testthat::expect_snapshot()
})

test_that("线图", {
  mtcars |>
    count(cyl) |>
    task_plotly_line(x = "cyl", y = "n") |>
    testthat::expect_snapshot()
})

test_that("面积图", {
  mtcars |>
    count(cyl) |>
    task_plotly_area(x = "cyl", y = "n") |>
    testthat::expect_snapshot()

  mtcars |> group_by(cyl) |>
    summarise(displ = mean(disp)) |>
    task_plotly_area(x = "cyl", y = "displ") |>
    testthat::expect_snapshot()
})

test_that("散点图", {
  mtcars |> task_plotly_marker(x = "mpg", y = "disp", alpha = 0.6) |>
    testthat::expect_snapshot()
})

test_that("直方图", {
  mtcars |> task_plotly_hist(x = "mpg") |>
    testthat::expect_snapshot()
})

test_that("极坐标柱图", {
  mtcars |>
    count(cyl) |>
    mutate(cyl = sprintf("CYL: %d", cyl)) |>
    task_plotly_barpolar(theta = "cyl", r = "n") |>
    testthat::expect_snapshot()
})
