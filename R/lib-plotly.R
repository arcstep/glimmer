#' @title 直方图
#' @examples 
#' mtcars |> plot_hist(x = "mpg")
#' @family lib-plotly functions
#' @export
plot_hist <- function(d, x, color = "brown", xtitle = NULL, ytitle = NULL) {
  plot_ly(x = as.formula(paste("~", x))) |>
    add_histogram(
      data = d |> collect(),
      color = I(color),
      name = paste(x, "")) |>
    layout(
      xaxis = list("title" = xtitle %empty% x),
      yaxis = list("title" = ytitle %empty% "数量"))
}


#' @title 散点图
#' @family lib-plotly functions
#' @examples 
#' mtcars |>
#'  plot_marker(x = "mpg", y = "disp", alpha = 0.6)
#' @export
plot_marker <- function(
    d, x, y, alpha = 0.2, name = NULL) {
  d |>
    plot_ly(x = as.formula(paste("~", x)), y = as.formula(paste("~", y))) |>
    add_markers(alpha = alpha, name = name %empty% "alpha")
}

#' @title 柱图
#' @family lib-plotly functions
#' @examples 
#' mtcars |>
#'   count(cyl) |>
#'   plot_bar(x = "cyl", y = "n")
#' 
#' mpg |> count(class) |>
#'   mutate(class = forcats::fct_reorder(class, n, .desc = TRUE)) |>
#'   plot_bar(x = "class", y = "n")
#' @export
plot_bar <- function(
    d, x, y) {
  d |>
    plot_ly() |>
    add_bars(x = as.formula(paste("~", x)), y = as.formula(paste("~", y)))
}

#' @title 线图
#' @examples 
#' mtcars |>
#'   count(cyl) |> 
#'   plot_line(x = "cyl", y = "n")
#' 
#' @family lib-plotly functions
#' @export
plot_line <- function(
    d, x, y,
    fillcolor = "red", linecolor = "rgb(205, 12, 24)", shape = "spline") {
  d |>
    plot_ly(x = as.formula(paste("~", x)), y = as.formula(paste("~", y))) |>
    add_lines(
      fillcolor = I(fillcolor),
      line = list(shape = shape, color = linecolor))
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
#'   plot_area(x = "cyl", y = "displ")
#'   
#' @export
plot_area <- function(
    d, x, y,
    fill = "tozeroy", color = "red", shape = "spline") {
  d |>
    plot_ly() |>
    add_trace(
      x = as.formula(paste("~", x)),
      y = as.formula(paste("~", y)),
      type = "scatter",
      mode = "lines",
      fill = fill,
      color = I(color), 
      line = list(shape = shape, width = 1))
}

#' @title 饼图
#' @examples 
#' mtcars |> plot_pie(value = "cyl")
#' @family lib-plotly functions
#' @export
plot_pie <- function(d, value, label = NULL, hole = 0.4) {
  d |> 
    plot_ly() |>
    add_pie(
      values = as.formula(paste("~", value)),
      labels = as.formula(paste("~", label %empty% value)),
      hole = hole)
}

#' @title 饼图
#' @family lib-plotly functions
#' @examples 
#' mtcars |> count(cyl) |>
#'  plot_pie2(value = "n", pull = c(0.1, 0, 0))
#' @export
plot_pie2 <- function(d, value, label = NULL, pull = 0, hole = 0.25) {
  d |> 
    plot_ly() |>
    add_trace(
      type = "pie",
      marker = list(line = list(width = 3, color = "black")),
      values = as.formula(paste("~", value)),
      labels = as.formula(paste("~", label %empty% value)),
      hole = hole, pull = pull)
}

#' @title 极坐标柱
#' @examples 
#' mtcars |>
#'   count(cyl) |>
#'   mutate(cyl = sprintf("CYL: %d", cyl)) |>
#'   plot_barpolar(theta = "cyl", r = "n")
#'   
#' @family lib-plotly functions
#' @export
plot_barpolar <- function(d, theta, r, color = NULL, bargap = 0, hole = 0.05, direction = "clockwise") {
  d |> 
    plot_ly() |>
    add_trace(
      type = "barpolar",
      theta = as.formula(paste("~", theta)),
      r = as.formula(paste("~", r)),
      color = color %empty% as.formula(paste("~", theta))) |>
    layout(
      polar = list(
        bargap = bargap,
        hole = hole,
        radialaxis = list("visible" = F),
        angularaxis = list("rotation" = 90 - 360/20/2, "direction" = direction, "visible" = F)
      ))
}
