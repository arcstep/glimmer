mod_preview_DT_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      uiOutput(ns("columns")),
      action_button(ns("search"), "查询"),
      action_button(ns("select-all"), "选择所有"),
      action_button(ns("deselect-all"), "仅前3个")
    ),
    tabset(
      tabs = list(
        list(menu = "摘要", id = ns("sum-tab"), content = uiOutput(ns("summary"))),
        list(menu = "明细", id = ns("detail-tab"), content = semantic_DTOutput(ns("DT")))),
      active = ns("detail-tab"),
      id = ns("DT-tab")
    )
  )
}
#
mod_preview_DT_server <- function(id, data, varId = "") {
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
    dsVal <- reactive({
      print(input[[nsServer("selected")]])
      selectedColumnVal(input[[nsServer("selected")]])
      data |> ds_select(columns = selectedColumnVal())
    }) |> bindEvent(input$search, ignoreNULL = T)
    #
    observe({
      selectedColumnVal(names(data))
    }) |> bindEvent(input$`select-all`, ignoreNULL = T, ignoreInit = T)
    #
    observe({
      selectedColumnVal(names(data) |> head(3))
    }) |> bindEvent(input$`deselect-all`, ignoreNULL = T, ignoreInit = T)
    # cards-items-ui
    items <- names(dsVal()) |> purrr::map(function(itemName) {
      columnClass <- class(dsVal()[[itemName]]) |> paste(collapse = ", ")
      columnCount <- length(unique(dsVal()[[itemName]]))
      if(NA %in% dsVal()[[itemName]]) {
        flagUI <- a(class = "ui red ribbon label", style = "margin-bottom: 5px;", "NA")
        desc <- sprintf("%s - 包含NA", columnCount)
      } else {
        if(columnCount == nrow(dsVal())) {
          flagUI <- a(class = "ui blue ribbon label", style = "margin-bottom: 5px;", "Unique")
        } else {
          flagUI <- NULL
        }
        desc <- sprintf("%d 个值", columnCount)
      }
      card(
        div(
          class = "content",
          flagUI,
          div(class = "header", itemName),
          div(class = "meta", columnClass),
          div(class = "description", desc)
        )
      )
    })
    #
    output$summary <- renderUI({
      infoUI <- sprintf("%d行, %d列", nrow(dsVal()), ncol(dsVal()))
      tagList(
        div(
          class = "ui raised segment",
          div(
            a(class = "ui green ribbon label", style = "margin-bottom: 5px;", "数据框"),
            infoUI
          )),
        cards(class = "five", !!!items))
    })
    #
    output$DT <- DT::renderDataTable({
      dsVal() |> semantic_DT(
        extensions = 'FixedColumns',
        options = list(
          dom = 't',
          scrollX = TRUE,
          fixedColumns = F
        ))
    })
  })
}
