test_that("直方图", {
  mtcars |> gali_plot_hist(e_x = "mpg") |>
    testthat::expect_snapshot()
})

test_that("散点图", {
  mtcars |> gali_plot_marker(e_x = "mpg", e_y = "disp", f1_alpha = 0.6) |>
    testthat::expect_snapshot()
})

test_that("饼图", {
  mtcars |>
    gali_plot_pie(e_value = "cyl") |>
    testthat::expect_snapshot()
  
  mtcars |> count(cyl) |>
   gali_plot_pie2(e_value = "n", f1s_pull = c(0.1, 0, 0)) |>
    testthat::expect_snapshot()
})

test_that("柱图", {
  mtcars |>
    count(cyl) |>
    gali_plot_bar(e_x = "cyl", e_y = "n") |>
    testthat::expect_snapshot()
})

test_that("线图", {
  mtcars |>
    count(cyl) |>
    gali_plot_line(e_x = "cyl", e_y = "n") |>
    testthat::expect_snapshot()
})

test_that("面积图", {
  mtcars |>
    count(cyl) |>
    gali_plot_area(e_x = "cyl", e_y = "n") |>
    testthat::expect_snapshot()

  mtcars |> group_by(cyl) |>
    summarise(displ = mean(disp)) |>
    gali_plot_area(e_x = "cyl", e_y = "displ") |>
    testthat::expect_snapshot()
})

test_that("极坐标柱图", {
  mtcars |>
    count(cyl) |>
    mutate(cyl = sprintf("CYL: %d", cyl)) |>
    gali_plot_barpolar(e_theta = "cyl", e_r = "n") |>
    testthat::expect_snapshot()
})
