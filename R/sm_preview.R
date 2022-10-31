#' @title Shiny Modules UI - 结果预览
#' @family Shiny Modules functions
#' @export
sm_preview_ui <- function(id) {
  ns <- NS(id)
  div(
    shiny::textOutput(ns("summarise")),
    hr(),
    DT::DTOutput(ns("dt_preview"))
  )
}

#' @title Shiny Modules Server - 结果预览
#' @family Shiny Modules functions
#' @export
sm_preview_server <- function(id, content, title = "") {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    if("data.frame" %in% class(content)) {
      output$summarise <- shiny::renderText(paste(title, " | ", nrow(content), "行，", ncol(content), "列"))
      output$dt_preview <- DT::renderDT({
        content |> DT::datatable()
      })
    }
  })
}