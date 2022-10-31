#' @title Shiny Modules UI - 任务脚本出参映射
#' @family Shiny Modules functions
#' @export
sm_output_assign_ui <- function(id, outputAssign) {
  ns <- NS(id)
  tags$div(
    if(!identical(NA, outputAssign)) {
      tags$ul(
        !!!(names(outputAssign) |> purrr::map(function(j) {
          tags$li( tags$span(j), ":", tags$span(params[[j]]))
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
  })
}