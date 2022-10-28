#' @export
sm_preview_ui <- function(id) {
  ns <- NS(id)
  div(
    shiny::textOutput(ns("summarise")),
    hr(),
    DT::DTOutput(ns("dt_preview"))
  )
}

#' @export
sm_preview_server <- function(id, content) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    if("data.frame" %in% class(content)) {
      output$summarise <- shiny::renderText(paste(nrow(content), "行，", ncol(content), "列"))
      output$dt_preview <- DT::renderDT({
        content |> DT::datatable()
      })
    }
  })
}