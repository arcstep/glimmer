library(shiny)
library(glimmer)

setwd("../../")
source("tests/testthat/helper.R")
sample_init()

task_create(taskId = "demo-task") |>
  task_item_add(type = "func", script = "ds_demo", params = list(demoDataset = "mtcars")) |>
  task_item_add(type = "func", script = "ds_arrange", params = list(columns = "disp", desc = TRUE))

#' @export
sm_task_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    uiOutput(ns("head")),
    actionButton(ns("run"), label = "执行", class = "btn btn-primary"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("task_params")),
        uiOutput(ns("task_items")),
        uiOutput(ns("task_env"))
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
    items <- taskInfo$items |> tibble::rowid_to_column()
    print(taskInfo)
    output$head <- renderUI({
      shiny::tags$h3(taskInfo$title %empty% taskInfo$taskId)
    })
    ##
    observeEvent(input$run, {
      resp <- task_run(taskId)
      print(resp)
      sm_preview_server("preview", resp)
    }, ignoreInit = TRUE)
    ##
    output$task_items <- renderUI({
      ## 子任务脚本清单
      shiny::tags$ol(
        !!!(items |> purrr::pmap(function(rowid, type, script, params, globalVars, inputAsign, outputAsign) {
          shiny::tags$li(
            span(paste0(type)),
            br(),
            shiny::tags$code(script |> as.character()),
            if(!identical(NA, params)) {
              ## 参数清单
              shiny::tags$ul(
                !!!(names(params) |> purrr::map(function(j) {
                  shiny::tags$li(
                    shiny::tags$span(j),
                    ":",
                    shiny::tags$span(params[[j]])
                  )
                }))
              )
            },
            p()
          )
        }))
      )
    })
  })
}

ui <- fluidPage(
  sm_task_ui("task")
)

server <- function(input, output, session) {
  sm_task_server("task", taskId = "demo-task")
}

shinyApp(ui, server)