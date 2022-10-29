#' @export
sm_params_ui <- function(id, params) {
  message("sm_params_ui:", id)
  ns <- NS(id)
  div(
    textOutput(ns("title")),
    if(!identical(NA, params)) {
      ## 参数清单
      tags$ul(
        !!!(names(params) |> purrr::map(function(j) {
          tags$li( tags$span(j), ":", tags$span(params[[j]]))
        }))
      )
    } else {
      tags$span("Empty")
    }
  )
}

#' @title 参数清单
#' @export
sm_params_server <- function(id, params) {
  moduleServer(id, function(input, output, session) {

  })
}