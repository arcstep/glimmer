#' @title Shiny Modules UI - 任务脚本出参映射
#' @family Shiny Modules functions
#' @export
sm_output_assign_ui <- function(id, outputAssign) {
  ns <- NS(id)
  tags$div(
    textOutput(ns("title")),
    if(!identical(NA, outputAssign)) {
      tags$ul(
        !!!(outputAssign |> purrr::map(function(j) {
          tags$li( tags$span(j) )
        }))
      )
    }
  )
}

#' @title Shiny Modules Server - 任务脚本出参映射
#' @family Shiny Modules functions
#' @export
sm_output_assign_server <- function(id, outputAssign) {
  moduleServer(id, function(input, output, session) {
    output$title <- renderText({
      if(!identical(NA, outputAssign)) {
        paste("- ", length(outputAssign), "outputAssigns:")
      } else {
        "- 0 outputAssign"
      }      
    })
  })
}