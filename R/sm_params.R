#' @export
sm_params_ui <- function(id, params) {
  message("sm_params_ui:", id)
  ns <- NS(id)
  tags$div(
    textOutput(ns("title")),
    if(!identical(NA, params)) {
      ## 参数清单
      tags$ul(
        !!!(names(params) |> purrr::map(function(j) {
          tags$li( tags$span(j), ":", tags$span(params[[j]]))
        }))
      )
    }
  )
}

#' @title 参数清单
#' @export
sm_params_server <- function(id, params) {
  moduleServer(id, function(input, output, session) {
    message("sm_params_server:", id)
    output$title <- renderText({
      if(!identical(NA, params)) {
        paste(length(params), "Params:")
      } else {
        "No Params"
      }      
    })
  })
}