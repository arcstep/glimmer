#' @title Shiny Modules UI - 任务脚本全局变量
#' @family Shiny Modules functions
#' @export
sm_global_vars_ui <- function(id, globalVars) {
  ns <- NS(id)
  tags$div(
    textOutput(ns("title")),
    if(!identical(NA, globalVars)) {
      message("globalVars: ",globalVars)
      ## 参数清单
      tags$ul(
        !!!(names(globalVars) |> purrr::map(function(j) {
          tags$li( tags$span(j), ":",
                   tags$span("[", class(globalVars[[j]]) |> as.character() |> paste(collapse = ","), "]")
                   )
        }))
      )}
  )
}

#' @title Shiny Modules Server - 任务脚本全局变量
#' @family Shiny Modules functions
#' @export
sm_global_vars_server <- function(id, globalVars) {
  moduleServer(id, function(input, output, session) {
    output$title <- renderText({
      if(!identical(NA, globalVars)) {
        paste("- ", length(globalVars), "globalVars:")
      } else {
        "- 0 globalVars"
      }      
    })
  })
}