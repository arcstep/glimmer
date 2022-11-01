#' @title Shiny Modules UI - 任务管理
#' @family Shiny Modules functions
#' @export
sm_task_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    ##
    uiOutput(ns("head")),
    wellPanel(uiOutput(ns("task-action"))),
    ##
    sm_choose_task_ui(ns("choose-task")),
    hr(),
    sidebarLayout(
      sidebarPanel(
        ## 任务信息
        uiOutput(ns("task-info")),
        ## 脚本清单
        uiOutput(ns("task-scripts-items")),
        ## 执行环境
        uiOutput(ns("task-env"))
      ),
      mainPanel(
        ## 结果预览
        sm_preview_ui(ns("preview"))
      )
    ))
}

#' @title Shiny Modules Server - 任务管理
#' @family Shiny Modules functions
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
        info <- taskInfo()$title %empty% taskId()
        if(!is.null(taskInfo()$snapId)) {
          info <- paste(info, "- [editing]")
        }
        shiny::tags$h3(info)
      }
    })
    # 更新编辑工具栏
    basicActions1 <- tagList(
      actionButton(ns("run"), label = "执行", class = "btn btn-primary", style = "margin-right:10px")
    )
    basicActions2 <- tagList(
      span("-", style = "margin-left:15px;margin-right:15px"),
      actionButton(ns("clone"), label = "克隆", class = "btn"),
      actionButton(ns("create"), label = "新建", class = "btn")
    )
    output$`task-action` <- renderUI({
      if(!is.null(taskInfo())) {
        if(is.null(taskInfo()$snapId)) {
          tagList(
            basicActions1,
            actionButton(ns("edit_snap"), label = "编辑", class = "btn"),
            basicActions2
          )
        } else {
          tagList(
            basicActions1,
            actionButton(ns("cancel_snap"), label = "取消编辑", class = "btn"),
            actionButton(ns("discard"), label = "复原", class = "btn"),
            actionButton(ns("save"), label = "保存", class = "btn"),
            actionButton(ns("submit"), label = "提交", class = "btn"),
            basicActions2
          )
        }
      }
    })
    # 更新任务内容
    observe({
      if(!is.null(taskInfo())) {
        message(taskInfo())
        items <- taskInfo()$items |> tibble::rowid_to_column()
        output$`task-scripts-items` <- renderUI({ sm_scripts_ui(ns("scripts"), items) })
        sm_scripts_server("scripts", items)
      }
    })
    # 执行任务
    observeEvent(input$run, {
      resp <- task_run(taskId())
      print(resp)
      sm_preview_server("preview", resp, title = paste("运行结果: ", taskId()))
    }, ignoreInit = TRUE)
    # 编辑任务
    observeEvent(input$edit_snap, {
      task_edit_snap(taskId())
      taskInfo(task_read(taskId(), snap = TRUE))
    }, ignoreInit = TRUE)
    # 保存任务
    observeEvent(input$save, {
      task_save(taskId())
      taskInfo(task_read(taskId(), snap = TRUE))
    }, ignoreInit = TRUE)
    # 提交任务
    observeEvent(input$submit, {
      task_submit(taskId())
      taskInfo(task_read(taskId()))
    }, ignoreInit = TRUE)
    # 复原任务
    observeEvent(input$discard, {
      task_discard(taskId())
      taskInfo(task_read(taskId(), snap = TRUE))
    }, ignoreInit = TRUE)
    # 取消编辑
    observeEvent(input$cancel_snap, {
      task_cancel_snap(taskId())
      taskInfo(task_read(taskId(), snap = TRUE))
    }, ignoreInit = TRUE)
  })
}