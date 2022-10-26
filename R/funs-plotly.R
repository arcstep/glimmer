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
plot_init <- function() {
  plotly::plot_ly() |>
    plotly::config(displaylogo = FALSE, locale = "zh-CN")
}

#' @title 绘制图层
#' @family lib-plotly functions
#' @export
plot_trace <- function(p, type, ..., data = NULL, inherit = TRUE) {
  plotly::add_trace(p, type, ..., data = NULL, inherit = TRUE)
}


plot_config_schema <- function() {
  list(
    "locale" = plotly_schema$config$locale,
    "displaytModeBar" = plotly_schema$config$displayModeBar,
    "modeBarButtons" = plotly_schema$config$modeBarButtons
  )
}

plot_layout_schema <- function() {
  list(
    "title" = plotly_schema$layout$layoutAttributes$title,
    "font" = plotly_schema$layout$layoutAttributes$font,
    "colorway" = plotly_schema$layout$layoutAttributes$colorway,
    "annotations" = plotly_schema$layout$layoutAttributes$annotations,
    "shapes" = plotly_schema$layout$layoutAttributes$shapes,
    "xaxis" = plotly_schema$layout$layoutAttributes$xaxis,
    "yaxis" = plotly_schema$layout$layoutAttributes$yaxis,
    "polar" = plotly_schema$layout$layoutAttributes$polar,
    "geo" = plotly_schema$layout$layoutAttributes$geo,
    "showlegend" = plotly_schema$layout$layoutAttributes$showlegend,
    "leagend" = plotly_schema$layout$layoutAttributes$legend,
    "width" = plotly_schema$layout$layoutAttributes$width,
    "height" = plotly_schema$layout$layoutAttributes$height,
    "plot_bgcolor" = plotly_schema$layout$layoutAttributes$plot_bgcolor,
    "paper_bgcolor" = plotly_schema$layout$layoutAttributes$paper_bgcolor
  )
}

plot_trace_schema <- function() {
  list(
    "bar" = list(
      attr = plotly_schema$traces[["bar"]]$attributes[
        c("x", "y", "text", "textposition", "marker", "name")],
      layoutAttr = plotly_schema$traces[["bar"]]$layoutAttributes[
        c("bargap", "barmode")]
    ),
    "barpolar" = list(
      attr = plotly_schema$traces[["barpolar"]]$attributes[
        c("r", "theta", "name")],
      layoutAttr = plotly_schema$traces[["barpolar"]]$layoutAttributes[
        c("bargap", "barmode")]
    ),
    "box" = list(
      attr = plotly_schema$traces[["box"]]$attributes[
        c("x", "y", "jitter", "boxpoints", "marker", "line", "name")]
    ),
    "carpet" = list(
      attr = plotly_schema$traces[["carpet"]]$attributes[
        c("a", "b", "y", "color", "name")]
    ),
    "contour" = list(
      attr = plotly_schema$traces[["contour"]]$attributes[
        c("x", "y", "z", "colorscale", "contours", "name")]
    ),
    "funnel" = list(
      attr = plotly_schema$traces[["funnel"]]$attributes[
        c("x", "y", "orientation", "textposition", "textinfo", "opacity", "marker", "connector", "name")]
    ),
    "funnelarea" = list(
      attr = plotly_schema$traces[["funnelarea"]]$attributes[
        c("textposition", "textinfo", "opacity", "marker", "name", "scalegroup")]
    ),
    "heatmap" = list(
      attr = plotly_schema$traces[["heatmap"]]$attributes[
        c("x", "y", "z", "colorscale", "name")]
    ),
    "histogram" = list(
      attr = plotly_schema$traces[["histogram"]]$attributes[
        c("x", "y", "histfunc", "name")],
      layoutAttr = plotly_schema$traces[["histogram"]]$layoutAttributes[
        c("barmode", "bargap")]
    ),
    "histogram2d" = list(
      attr = plotly_schema$traces[["histogram2d"]]$attributes[
        c("x", "y", "histfunc", "name")]
    ),
    "histogram2dcontour" = list(
      attr = plotly_schema$traces[["histogram2dcontour"]]$attributes[
        c("x", "y", "histfunc", "contours", "name")]
    ),
    "icicle" = list(
      attr = plotly_schema$traces[["icicle"]]$attributes[
        c("labels", "parents", "ids", "name")]
    ),
    "indicator" = list(
      attr = plotly_schema$traces[["indicator"]]$attributes[
        c("domain", "value", "delta", "gauge", "title", "mode")]
    ),
    "pie" = list(
      attr = plotly_schema$traces[["pie"]]$attributes[
        c("textposition", "textinfo", "insidetextfont", "hoverinfo", "text",
          "values", "hole", "marker", "pull", "name")]
    ),
    "sankey" = list(
      attr = plotly_schema$traces[["sankey"]]$attributes[
        c("orientation", "node", "link")]
    ),
    "scatter" = list(
      attr = plotly_schema$traces[["scatter"]]$attributes[
        c("x", "y", "text", "marker", "mode", "hoverinfo")]
    ),
    "scatterpolar" = list(
      attr = plotly_schema$traces[["scatterpolar"]]$attributes[
        c("r", "theta", "text", "marker", "mode", "hoverinfo")]
    ),
    "sunburst" = list(
      attr = plotly_schema$traces[["sunburst"]]$attributes[
        c("values", "parents", "labels", "branchvalues", "maxdepth", "insidetextorientation")]
    ),
    "table" = list(
      attr = plotly_schema$traces[["table"]]$attributes[
        c("header", "cells")]
    ),
    "treemap" = list(
      attr = plotly_schema$traces[["treemap"]]$attributes[
        c("values", "parents", "labels", "textinfo", "outsidetextfont", "marker",
          "pathbar", "maxdepth", "ids")]
    ),
    "violin" = list(
      attr = plotly_schema$traces[["violin"]]$attributes[
        c("x", "y", "marker", "line", "name")]
    ),
    "waterfall" = list(
      attr = plotly_schema$traces[["waterfall"]]$attributes[
        c("x", "y", "measure", "orientation", "connector")]
    )
  )
}

#' @export
plot_trace_schema_yaml <- function() {
  x <- plot_trace_schema()
  objects <- list()
  names(x) |> purrr::walk(function(i) {
    names(x[[i]]$attr) |> purrr::walk(function(j) {
      if(!is.na(j)) {
        if("valType" %nin% names(x[[i]]$attr[[j]])) {
          x[[i]]$attr[[j]] <<- NULL
          x[[i]]$attr[[j]] <<- list(
            valType = "object"
          )
          if(j %in% names(objects)) {
            objects[[j]] <<- unlist(c(objects[[j]], i))
          } else {
            objects[[j]] <<- i
          }
        }
      } else {
        warning("No attr: ", i, "$", j)
      }
    })
  })
  x |> yaml::write_yaml("data/plot-trace-schema.yml")
  objects
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
