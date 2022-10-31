#' @title Shiny Modules UI - 任务脚本入参映射
#' @family Shiny Modules functions
#' @export
sm_input_assign_ui <- function(id, inputAssign) {
  ns <- NS(id)
  tags$div(
    if(!identical(NA, inputAssign)) {
      tags$ul(
        !!!(names(inputAssign) |> purrr::map(function(j) {
          tags$li( tags$span(j), ":", tags$span(params[[j]]))
        }))
      )
    }
  )
}

#' @title Shiny Modules Server - 任务脚本入参映射
#' @family Shiny Modules functions
#' @export
sm_input_assign_server <- function(id, inputAssign) {
  moduleServer(id, function(input, output, session) {
  })
}