#' @title Shiny Modules UI - 任务脚本全局变量
#' @family Shiny Modules functions
#' @export
sm_global_vars_ui <- function(id, globalVars) {
  ns <- NS(id)
  tags$div(
    if(!identical(NA, globalVars)) {
      ## 参数清单
      tags$ul(
        !!!(names(globalVars) |> purrr::map(function(j) {
          tags$li( tags$span(j), ":", tags$span(params[[j]]))
        }))
      )
    }
  )
}

#' @title Shiny Modules Server - 任务脚本全局变量
#' @family Shiny Modules functions
#' @export
sm_global_vars_server <- function(id, globalVars) {
  moduleServer(id, function(input, output, session) {
  })
}