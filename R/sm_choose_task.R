#' @export
sm_choose_task_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    selectInput(ns("choose-task"),
                label = "任务列表",
                selected = "demo-task",
                choices = task_search()$taskId)
  )
}

#' @export
sm_choose_task_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(input$`choose-task`)
  })
}
