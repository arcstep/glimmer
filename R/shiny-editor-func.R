#' @title 选择函数和参数
#' @export
funcEditorUI <- function(id) {
  ns <- NS(id)
  wellPanel(
    shiny::selectInput(
      ns("funs_topic"),
      label = "选择函数域",
      selected = "ds",
      choices = get_funs_schema()$items),
    uiOutput(ns("func_selected"))
  )
}

#' @export
funcEditorServer <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    output$func_selected <- renderUI({
      shiny::selectInput(
        ns("func_name"),
        label = "选择函数名称", 
        choices = get_funs_schema(input$funs_topic, "functions")$items)
    })
  })
}