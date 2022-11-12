mod_preview_DT_select_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      uiOutput(ns("columns")),
      action_button(ns("search"), "查询"),
      action_button(ns("select-all"), "选择所有"),
      action_button(ns("deselect-all"), "仅前3个")
    )
  )
}
#
mod_preview_DT_select_server <- function(id, data, varId = "") {
  moduleServer(id, function(input, output, session) {
    # 确保动态创建DOM唯一性
    nsUI <- NS(session$ns(varId))
    nsServer <- NS(varId)
    #
    output$columns <- renderUI({
      div(
        class = "option-checkbox",
        multiple_checkbox(
          nsUI("selected"),
          label = "选择列",
          choices = names(data),
          selected = selectedColumnVal() %empty% names(data),
          position = "inline")
      )
    })
    #
    selectedColumnVal <- reactiveVal()
    #
    observe({
      selectedColumnVal(names(data))
    }) |> bindEvent(input$`select-all`, ignoreNULL = T, ignoreInit = T)
    #
    observe({
      selectedColumnVal(names(data) |> head(3))
    }) |> bindEvent(input$`deselect-all`, ignoreNULL = T, ignoreInit = T)
    #
    dsVal <- reactive({
      print(input[[nsServer("selected")]])
      selectedColumnVal(input[[nsServer("selected")]])
      data |> ds_select(columns = selectedColumnVal())
    }) |> bindEvent(input$search, ignoreNULL = T)
    
    #
    return(dsVal)
  })
}