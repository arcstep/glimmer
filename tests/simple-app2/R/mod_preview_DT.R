mod_preview_DT_ui <- function(id) {
  ns <- NS(id)
  tagList(
    ## 选列
    mod_preview_DT_select_ui(ns("DT-select")),
    tabset(
      tabs = list(
        ##
        list(menu = "摘要", id = ns("sum-tab"), content = mod_preview_DT_summary_ui(ns("summary"))),
        ##
        list(menu = "明细", id = ns("detail-tab"), content = semantic_DTOutput(ns("DT")))),
      active = ns("detail-tab"),
      id = ns("DT-tab")
    )
  )
}
#
mod_preview_DT_server <- function(id, data, varId = "") {
  moduleServer(id, function(input, output, session) {
    # 选列
    dsVal <- mod_preview_DT_select_server("DT-select", data, varId)
    # 摘要
    mod_preview_DT_summary_server("summary", dsVal)
    # 明细
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
