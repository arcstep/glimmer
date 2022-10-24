#' @title 直方图
#' @examples 
#' mtcars |> gali_plot_hist(e_x = "mpg")
#' @family lib-plotly functions
#' @export
gali_plot_hist <- function(`@ds`, e_x, c_color = "brown", s_xtitle = NULL, s_ytitle = NULL) {
  plot_ly(x = as.formula(paste("~", e_x))) |>
    add_histogram(
      data = `@ds` |> collect(),
      color = I(c_color),
      name = paste(e_x, "")) |>
    layout(
      xaxis = list("title" = s_xtitle %empty% e_x),
      yaxis = list("title" = s_ytitle %empty% "数量"))
}


#' @title 散点图
#' @family lib-plotly functions
#' @examples 
#' mtcars |>
#'  plot_marker(e_x = "mpg", e_y = "disp", f1_alpha = 0.6)
#' @export
gali_plot_marker <- function(`@ds`, e_x, e_y, f1_alpha = 0.2, s_name = NULL) {
  `@ds` |>
    plot_ly(x = as.formula(paste("~", e_x)), y = as.formula(paste("~", e_y))) |>
    add_markers(alpha = f1_alpha, name = s_name %empty% "alpha")
}

#' @title 柱图
#' @family lib-plotly functions
#' @examples 
#' mtcars |>
#'   count(cyl) |>
#'   gali_plot_bar(e_x = "cyl", e_y = "n")
#' 
#' mpg |> count(class) |>
#'   mutate(class = forcats::fct_reorder(class, n, .desc = TRUE)) |>
#'   plot_bar(x = "class", y = "n")
#' @export
gali_plot_bar <- function(`@ds`, e_x, e_y) {
  `@ds` |>
    plot_ly() |>
    add_bars(x = as.formula(paste("~", e_x)), y = as.formula(paste("~", e_y)))
}

#' @title 线图
#' @examples 
#' mtcars |>
#'   count(cyl) |> 
#'   gali_plot_line(e_x = "cyl", e_y = "n")
#' 
#' @family lib-plotly functions
#' @export
gali_plot_line <- function(
    `@ds`, e_x, e_y,
    e_fillcolor = "red", c_linecolor = "rgb(205, 12, 24)", e_shape = "spline") {
  `@ds` |>
    plot_ly(x = as.formula(paste("~", e_x)), y = as.formula(paste("~", e_y))) |>
    add_lines(
      fillcolor = I(e_fillcolor),
      line = list(shape = e_shape, color = c_linecolor))
}

#' @title 面积图
#' @family lib-plotly functions
#' @examples 
#' mtcars |>
#'   count(cyl) |>
#'   plot_area(x = "cyl", y = "n")
#'   
#' mtcars |> group_by(cyl) |>
#'   summarise(displ = mean(disp)) |>
#'   gali_plot_area(e_x = "cyl", e_y = "displ")
#'   
#' @export
gali_plot_area <- function(
    `@ds`, e_x, e_y,
    c_fill = "tozeroy", c_color = "red", e_shape = "spline") {
  `@ds` |>
    plot_ly() |>
    add_trace(
      x = as.formula(paste("~", e_x)),
      y = as.formula(paste("~", e_y)),
      type = "scatter",
      mode = "lines",
      fill = c_fill,
      color = I(c_color), 
      line = list(shape = e_shape, width = 1))
}

#' @title 饼图
#' @examples 
#' mtcars |> gali_plot_pie(e_value = "cyl")
#' @family lib-plotly functions
#' @export
gali_plot_pie <- function(`@ds`, e_value, s_label = NULL, f1_hole = 0.4) {
  `@ds` |> 
    plot_ly() |>
    add_pie(
      values = as.formula(paste("~", e_value)),
      labels = as.formula(paste("~", s_label %empty% e_value)),
      hole = f1_hole)
}

#' @title 饼图
#' @family lib-plotly functions
#' @examples 
#' mtcars |> count(cyl) |>
#'  gali_plot_pie2(e_value = "n", pull = c(0.1, 0, 0))
#' @export
gali_plot_pie2 <- function(`@ds`, e_value, s_label = NULL, f1s_pull = 0, f1_hole = 0.25) {
  `@ds` |> 
    plot_ly() |>
    add_trace(
      type = "pie",
      marker = list(line = list(width = 3, color = "black")),
      values = as.formula(paste("~", e_value)),
      labels = as.formula(paste("~", s_label %empty% e_value)),
      hole = f1_hole,
      pull = f1s_pull)
}

#' @title 极坐标柱
#' @examples 
#' mtcars |>
#'   count(cyl) |>
#'   mutate(cyl = sprintf("CYL: %d", cyl)) |>
#'   gali_plot_barpolar(e_theta = "cyl", e_r = "n")
#'   
#' @family lib-plotly functions
#' @export
gali_plot_barpolar <- function(`@ds`, e_theta, e_r, c_color = NULL, i_bargap = 0, f1_hole = 0.05, e_direction = "clockwise") {
  `@ds` |> 
    plot_ly() |>
    add_trace(
      type = "barpolar",
      theta = as.formula(paste("~", e_theta)),
      r = as.formula(paste("~", e_r)),
      color = c_color %empty% as.formula(paste("~", e_theta))) |>
    layout(
      polar = list(
        bargap = i_bargap,
        hole = f1_hole,
        radialaxis = list("visible" = F),
        angularaxis = list("rotation" = 90 - 360/20/2, "direction" = e_direction, "visible" = F)
      ))
}
