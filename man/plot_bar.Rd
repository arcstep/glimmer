% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funs-plotly.R
\name{plot_bar}
\alias{plot_bar}
\title{柱图}
\usage{
plot_bar(d, e_x, e_y)
}
\description{
柱图
}
\examples{
mtcars |>
  count(cyl) |>
  gali_plot_bar(e_x = "cyl", e_y = "n")

mpg |> count(class) |>
  mutate(class = forcats::fct_reorder(class, n, .desc = TRUE)) |>
  plot_bar(x = "class", y = "n")
}
\seealso{
Other lib-plotly functions: 
\code{\link{plot_area}()},
\code{\link{plot_barpolar}()},
\code{\link{plot_hist}()},
\code{\link{plot_init}()},
\code{\link{plot_layout}()},
\code{\link{plot_line}()},
\code{\link{plot_marker}()},
\code{\link{plot_pie2}()},
\code{\link{plot_pie}()},
\code{\link{plot_trace}()}
}
\concept{lib-plotly functions}
