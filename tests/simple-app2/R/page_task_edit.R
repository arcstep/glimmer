page_task_edit_ui <- function(id) {
  ns <- NS(id)
  semanticPage(
    titlePanel("edit page"),
    segment(
      class = "basic",
      textOutput(ns("edit-taskId"))
    )
  )
}
page_task_edit_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$`edit-taskId` <- renderText(
      shiny.router::get_query_param("taskId")
    )
  })
}