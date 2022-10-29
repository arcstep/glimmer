#' @export
sm_task_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    ##
    uiOutput(ns("head")),
    actionButton(ns("run"), label = "执行", class = "btn btn-primary"),
    ##
    sm_choose_task_ui(ns("choose-task")),
    hr(),
    sidebarLayout(
      sidebarPanel(
        ##
        uiOutput(ns("task-info")),
        ##
        uiOutput(ns("task-scripts-items")),
        ##
        uiOutput(ns("task-env"))
      ),
      mainPanel(
        ##
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
    ## 切换任务
    observe({
      taskId(sm_choose_task_server("choose-task"))
      if(!is.null(taskId())) {
        taskId() |> print()
        taskInfo(task_read(taskId()))
      }
    })
    # 更新任务摘要
    output$head <- renderUI({
      if(!is.null(taskInfo())) {
        shiny::tags$h3(taskInfo()$title %empty% taskId())
      }
    })
    # 更新任务内容
    observe({
      if(!is.null(taskInfo())) {
        message(taskInfo())
        items <- taskInfo()$items |> tibble::rowid_to_column()
        sm_scripts_server("scripts", items)
        output$`task-scripts-items` <- renderUI({ sm_scripts_ui(ns("scripts"), items) })
      }
    })
    # 结果预览
    observeEvent(input$run, {
      resp <- task_run(taskId())
      print(resp)
      sm_preview_server("preview", resp)
    }, ignoreInit = TRUE)
  })
}
