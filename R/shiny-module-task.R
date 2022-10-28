#' @export
sm_ui_task <- function(id) {
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
        uiOutput(ns("preview"))
      )
    ))
}

#' @export
sm_server_task <- function(id, taskId, displayMode = "editor") {
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
    output$task_items <- renderUI({
      shiny::tags$ul(
        !!!(items |> purrr::pmap(function(rowid, type, script, params, inputAsign, outputAsign) {
          shiny::tagList(
            span(paste0(rowid, " ", type)),
            br(),
            if(length(script) > 0) {
              shiny::tags$code(script)
            } else {
              ""
            },
            shiny::tags$li(
              !!!(params |> purrr::map(function(item) {
                if(length(item) > 0) {
                  shiny::tags$code(item |> as.character())
                } else {
                  ""
                }
              }))
            ),
            p()
          )
        }))
      )
    })
  })
}