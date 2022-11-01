#' @title Shiny Modules UI - 任务脚本参数
#' @family Shiny Modules functions
#' @export
sm_params_ui <- function(id, params) {
  ns <- NS(id)
  tags$div(
    textOutput(ns("title")),
    if(!identical(NA, params)) {
      ## 参数清单
      tags$ul(
        !!!(names(params) |> purrr::map(function(j) {
          tags$li( tags$span(j), ":", tags$span(params[[j]]) )
        }))
      )
    }
  )
}

#' @title Shiny Modules Server - 任务脚本参数
#' @family Shiny Modules functions
#' @export
sm_params_server <- function(id, params) {
  moduleServer(id, function(input, output, session) {
    output$title <- renderText({
      if(!identical(NA, params)) {
        paste("- ", length(params), "Params:")
      } else {
        "- 0 Params"
      }      
    })
  })
}