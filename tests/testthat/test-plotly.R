test_that("饼图", {
  mtcars |>
    plot_pie(value = "cyl") |>
    testthat::expect_snapshot()
  
  mtcars |> count(cyl) |>
   plot_pie2(value = "n", pull = c(0.1, 0, 0)) |>
    testthat::expect_snapshot()
})

test_that("柱图", {
  mtcars |>
    count(cyl) |>
    plot_bar(x = "cyl", y = "n") |>
    testthat::expect_snapshot()
})

test_that("线图", {
  mtcars |>
    count(cyl) |>
    plot_line(x = "cyl", y = "n") |>
    testthat::expect_snapshot()
})

test_that("面积图", {
  mtcars |>
    count(cyl) |>
    plot_area(x = "cyl", y = "n") |>
    testthat::expect_snapshot()

  mtcars |> group_by(cyl) |>
    summarise(displ = mean(disp)) |>
    plot_area(x = "cyl", y = "displ") |>
    testthat::expect_snapshot()
})

test_that("散点图", {
  mtcars |> plot_marker(x = "mpg", y = "disp", alpha = 0.6) |>
    testthat::expect_snapshot()
})

test_that("直方图", {
  mtcars |> plot_hist(x = "mpg") |>
    testthat::expect_snapshot()
})

test_that("极坐标柱图", {
  mtcars |>
    count(cyl) |>
    mutate(cyl = sprintf("CYL: %d", cyl)) |>
    plot_barpolar(theta = "cyl", r = "n") |>
    testthat::expect_snapshot()
})
