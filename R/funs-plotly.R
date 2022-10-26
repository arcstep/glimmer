##
plotly_schema <- readRDS("data/plotly/plotly-schema.rds")

## 加载plotly配置
plotly_schema_from_yaml <- function(path = "data/plotly") {
  myschema <- list(layout = list(), traces = list(), config = list())
  myschema$config <- yaml::read_yaml(fs::path_join(c(path, "plotly-config.yml")))
  myschema$layout$layoutAttributes <- yaml::read_yaml(fs::path_join(c(path, "plotly-layout.yml")))
  traces <- yaml::read_yaml(fs::path_join(c(path, "plotly-traces.yml")))
  traces |> purrr::walk(function(item) {
    myschema$traces[[item]] <<- yaml::read_yaml(fs::path_join(c(path, paste0("plotly-", item, ".yml"))))
  })
  myschema |> saveRDS(fs::path_join(c(path, "plotly-schema.rds")))
}

## 生成plotly配置的yaml文件
plotly_schema_to_yaml <- function(path = "data/plotly") {
  myschema <- plotly::schema()
  fs::dir_create(path)
  myschema$config |> yaml::write_yaml(fs::path_join(c(path, "plotly-config.yml")))
  myschema$layout$layoutAttributes |> yaml::write_yaml(fs::path_join(c(path, "plotly-layout.yml")))
  myschema$traces |> names() |> yaml::write_yaml(fs::path_join(c(path, "plotly-traces.yml")))
  names(myschema$traces) |> purrr::walk(function(item) {
    myschema$traces[[item]] |> yaml::write_yaml(fs::path_join(c(path, paste0("plotly-", item, ".yml"))))
  })
}

#' @title 查询
#' @param plot plotly对象
#' @param type trace类型
#' @param paramName 参数名称
#' @export
get_params_plot <- function(entry = "plotly_schema", paramName = NULL, v = NULL, plot = NULL) {
  if(is.null(paramName)) {
    list(
      "valType" = "object",
      "description" = "-",
      "params" = names(parse(text = entry) |> eval()),
      "entry" = entry
    )
  } else {
    a <- paste0(entry, "$", paramName)
    param <- parse(text = a) |> eval()
    ## 所选参数在plotly的schema定义中直接可以找到
    if("valType" %in% names(param)) {
      ## 直接返回非列表参数
      resp <- param
      if(param$valType == "data_array" && !is.null(plot)) {
        ## 从plotly绑定的数据中提取可选列名
        resp$columns = names(plotly::plotly_data(plot))
      }
    } else {
      resp <- list(
        "valType" = "object",
        "description" = "-",
        "params" = names(param),
        "entry" = a
      )
    }
    resp$value <- v
    resp    
  }
}

#' @title 初始化plotly
#' @family lib-plotly functions
#' @export
plot0 <- function(d, ...) {
  d |> plotly::plot_ly(...)
}

#' @title 绘制坐标轴
#' @family lib-plotly functions
#' @export
plot_trace <- function(plot, type, ...) {
  plot |> plotly::add_trace(type = type, ...)
}

#' @title 绘制坐标轴
#' @family lib-plotly functions
#' @export
plot_layout <- function(plot, ...) {
  plot |> plotly::layout(type = type, ...)
}


#' @title 直方图
#' @examples 
#' mtcars |> gali_plot_hist(e_x = "mpg")
#' @family lib-plotly functions
#' @export
plot_hist <- function(d, e_x, c_color = "brown", s_xtitle = NULL, s_ytitle = NULL) {
  plot_ly(x = as.formula(paste("~", e_x))) |>
    add_histogram(
      data = d |> collect(),
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
plot_marker <- function(d, e_x, e_y, f1_alpha = 0.2, s_name = NULL) {
  d |>
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
plot_bar <- function(d, e_x, e_y) {
  d |>
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
plot_line <- function(
    d, e_x, e_y,
    e_fillcolor = "red", c_linecolor = "rgb(205, 12, 24)", e_shape = "spline") {
  d |>
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
plot_area <- function(
    d, e_x, e_y,
    c_fill = "tozeroy", c_color = "red", e_shape = "spline") {
  d |>
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
plot_pie <- function(d, e_value, s_label = NULL, f1_hole = 0.4) {
  d |> 
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
plot_pie2 <- function(d, e_value, s_label = NULL, f1s_pull = 0, f1_hole = 0.25) {
  d |> 
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
plot_barpolar <- function(d, e_theta, e_r, c_color = NULL, i_bargap = 0, f1_hole = 0.05, e_direction = "clockwise") {
  d |> 
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
