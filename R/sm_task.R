#' @export
sm_task_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    uiOutput(ns("head")),
    actionButton(ns("run"), label = "执行", class = "btn btn-primary"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("task-info")),
        uiOutput(ns("task-scripts-items")),
        uiOutput(ns("task-env"))
      ),
      mainPanel(
        sm_preview_ui(ns("preview"))
      )
    ))
}

#' @export
sm_task_server <- function(id, taskId, displayMode = "editor") {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    ##
    taskInfo <- task_read(taskId)
    print(taskInfo)
    ##
    output$head <- renderUI({shiny::tags$h3(taskInfo$title %empty% taskInfo$taskId)})
    ##
    scriptItems <- taskInfo$items |> tibble::rowid_to_column()
    output[["task-scripts-items"]] <- renderUI(sm_scripts_ui(ns("list"), scriptItems))
    ##
    observeEvent(input$run, {
      resp <- task_run(taskId)
      print(resp)
      sm_preview_server("preview", resp)
    }, ignoreInit = TRUE)
    ##
    sm_scripts_server("task-scripts", scriptItems)
  })
}
