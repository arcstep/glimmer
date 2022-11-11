
#
mod_preview_DT_ui <- function(id) {
  ns <- NS(id)
  tabset(
    tabs = list(
      list(
        menu = "摘要",
        id = ns("sum-tab"),
        content = uiOutput(ns("summary")) 
      ),
      list(
        menu = "明细",
        id = ns("detail-tab"),
        content = semantic_DTOutput(ns("DT"))
      )
    ),
    active = ns("detail-tab"),
    id = ns("DT-tab")
  )
}
#
mod_preview_DT_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # cards-items-ui
    items <- names(data) |> purrr::map(function(itemName) {
      card(
        div(
          class = "content",
          div(
            class = "header",
            itemName
          ),
          div(
            class = "meta",
            class(data[[itemName]])[1]
          ),
          div(
            class = "description",
            paste(length(unique(data[[itemName]])), "个值")
          )
        )
      )
    })
    #
    output$summary <- renderUI({
      tagList(
        div(
          class = "ui raised segment",
          div(
            a(class="ui green ribbon label", "数据框"),
            br(),
            p(sprintf("%d行, %d列", nrow(data), ncol(data)))
          )),
        cards(
          class = "two",
          !!!items
        ))
    })
    output$DT <- DT::renderDataTable({
      data |> semantic_DT()
    })
  })
}
