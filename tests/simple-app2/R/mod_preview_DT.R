mod_preview_DT_ui <- function(id) {
  ns <- NS(id)
  tabset(
    tabs = list(
      list(menu = "摘要", id = ns("sum-tab"), content = uiOutput(ns("summary"))),
      list(menu = "明细", id = ns("detail-tab"), content = semantic_DTOutput(ns("DT")))),
    active = ns("detail-tab"),
    id = ns("DT-tab")
  )
}
#
mod_preview_DT_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # cards-items-ui
    items <- names(data) |> purrr::map(function(itemName) {
      columnClass <- class(data[[itemName]]) |> paste(collapse = ", ")
      columnCount <- length(unique(data[[itemName]]))
      if(NA %in% data[[itemName]]) {
        flagUI <- a(class = "ui red ribbon label", style = "margin-bottom: 5px;", "NA")
        desc <- sprintf("%s - 包含NA", columnCount)
      } else {
        if(columnCount == nrow(data)) {
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
      infoUI <- sprintf("%d行, %d列", nrow(data), ncol(data))
      tagList(
        div(
          class = "ui raised segment",
          div(
            a(class = "ui green ribbon label", style = "margin-bottom: 5px;", "数据框"),
            infoUI
          )),
        cards(class = "four", !!!items))
    })
    output$DT <- DT::renderDataTable({
      data |> semantic_DT()
    })
  })
}
