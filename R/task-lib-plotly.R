#' @title 直方图
#' @family task-lib functions
#' @export
task_plotly_hist <- function(d, x, color = "brown", xtitle = NULL, ytitle = NULL) {
  plotly::plot_ly(x = as.formula(paste("~", x))) |>
    plotly::add_histogram(
      data = d |> collect(),
      color = I(color),
      name = paste(x, "")) |>
    plotly::layout(
      xaxis = list("title" = xtitle %empty% x),
      yaxis = list("title" = ytitle %empty% "数量"))
}


#' @title 散点图
#' @family task-lib functions
#' @examples 
#' mpg |> task_plotly_marker("cty", "hwy")
#' @export
task_plotly_marker <- function(
    d, x, y, alpha = 0.2, name = NULL) {
  d |>
    plotly::plot_ly(x = as.formula(paste("~", x)), y = as.formula(paste("~", y))) |>
    plotly::add_markers(alpha = alpha, name = name %empty% "alpha")
}

#' @title 柱图
#' @family task-lib functions
#' @examples 
#' mpg |> count(class) |>
#'   mutate(class = forcats::fct_reorder(class, n, .desc = TRUE)) |>
#'   task_plotly_bar(x = "class", y = "n")
#' @export
task_plotly_bar <- function(
    d, x, y) {
  d |>
    plotly::plot_ly() |>
    plotly::add_bars(x = as.formula(paste("~", x)), y = as.formula(paste("~", y)))
}

#' @title 线图
#' @family task-lib functions
#' @export
#' @examples 
#' 
task_plotly_line <- function(
    d, x, y,
    fillcolor = "red", linecolor = "rgb(205, 12, 24)", shape = "spline") {
  d |>
    plotly::plot_ly(x = as.formula(paste("~", x)), y = as.formula(paste("~", y))) |>
    plotly::add_lines(
      fillcolor = I(fillcolor),
      line = list(shape = shape, color = linecolor))
}

#' @title 面积图
#' @family task-lib functions
#' @examples 
#' mpg |> group_by(model) |>
#'  summarise(displ = mean(displ)) |>
#'  task_plotly_area(x = "model", y = "displ")
#' @export
task_plotly_area <- function(
    d, x, y,
    fill = "tozeroy", color = "red", shape = "spline") {
  d |>
    plotly::plot_ly() |>
    plotly::add_trace(
      x = as.formula(paste("~", x)),
      y = as.formula(paste("~", y)),
      type = "scatter",
      mode = "lines",
      fill = fill,
      color = I(color), 
      line = list(shape = shape, width = 1))
}

#' @title 饼图
#' @family task-lib functions
#' @export
task_plotly_pie <- function(d, value, label = NULL, hole = 0.4) {
  d |> 
    plotly::plot_ly() |>
    plotly::add_pie(
      values = as.formula(paste("~", value)),
      labels = as.formula(paste("~", label %empty% value)),
      hole = hole)
}

#' @title 饼图
#' @family task-lib functions
#' @examples 
#' mtcars |> count(cyl) |>
#'  task_plotly_pie2(value = "n", pull = c(0.1, 0, 0))
#' @export
task_plotly_pie2 <- function(d, value, label = NULL, pull = 0, hole = 0.25) {
  d |> 
    plotly::plot_ly() |>
    add_trace(
      type = "pie",
      marker = list(line = list(width = 3, color = "black")),
      values = as.formula(paste("~", value)),
      labels = as.formula(paste("~", label %empty% value)),
      hole = hole, pull = pull)
}

#' @title 极坐标柱
#' @family task-lib functions
#' @export
task_plotly_barpolar <- function(d, theta, r, color = NULL, bargap = 0, hole = 0.05, direction = "clockwise") {
  d |> 
    plotly::plot_ly() |>
    plotly::add_trace(
      type = "barpolar",
      theta = as.formula(paste("~", theta)),
      r = as.formula(paste("~", r)),
      color = color %empty% as.formula(paste("~", theta))) |>
    plotly::layout(
      polar = list(
        bargap = bargap,
        hole = hole,
        radialaxis = list("visible" = F),
        angularaxis = list("rotation" = 90 - 360/20/2, "direction" = direction, "visible" = F)
      ))
}
