#' @export
sm_task_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    uiOutput(ns("head")),
    actionButton(ns("run"), label = "执行", class = "btn btn-primary"),
    sm_choose_task_ui(ns("choose-task")),
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
    taskId <- reactiveVal(NULL)
    taskInfo <- reactiveVal(NULL)
    scriptItems <- reactiveVal(NULL)
    ##
    observe({
      taskId(sm_choose_task_server("choose-task"))
      if(!is.null(taskId())) {
        taskId() |> print()
        taskInfo(task_read(taskId()))
        scriptItems(taskInfo()$items |> tibble::rowid_to_column())
      }
    })
    #
    output$head <- renderUI({
      if(!is.null(taskInfo())) {
        shiny::tags$h3(taskInfo()$title %empty% taskInfo()$taskId)
      }
    })
    #
    output$`task-scripts-items` <- renderUI({
      if(!is.null(scriptItems())) {
        sm_scripts_ui(ns("scripts"), scriptItems())
      }
    })
    #
    observeEvent(input$run, {
      resp <- task_run(taskId())
      print(resp)
      sm_preview_server("preview", resp)
    }, ignoreInit = TRUE)
    #
    observe({
      if(!is.null(scriptItems())) {
        message(scriptItems())
        sm_scripts_server("scripts", scriptItems())
      }
    })
  })
}
