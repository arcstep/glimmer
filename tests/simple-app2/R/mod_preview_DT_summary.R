mod_preview_DT_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("content"))
  )
}
#
mod_preview_DT_summary_server <- function(id, dsVal) {
  moduleServer(id, function(input, output, session) {
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
    output$content <- renderUI({
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
  })
}
