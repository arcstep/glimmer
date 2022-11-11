page_task_show_ui <- function(id) {
  ns <- NS(id)
  semanticPage(
    titlePanel("show page"),
    segment(
      class = "basic",
      textOutput(ns("show-taskId")),
      action_button(ns("show-to-edit"), "toEdit")
    )
  )
}
page_task_show_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$`show-taskId` <- renderText(
      shiny.router::get_query_param("taskId")
    )
    observeEvent(input$`show-to-edit`, {
      taskId <- shiny.router::get_query_param("taskId")
      shiny.router::change_page(paste0("task/edit?taskId=", taskId))
    })
  })
}
