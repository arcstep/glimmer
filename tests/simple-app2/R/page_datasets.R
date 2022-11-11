page_datasets_ui <- function(id) {
  ns <- NS(id)
  semanticPage(
    titlePanel("所有数据集"),
    segment(
      class = "basic",
      semantic_DTOutput(ns("dataset-table"))
    )
  )
}
page_datasets_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    datasetToPreview <- reactiveVal(NULL)
    output$`dataset-table` <- DT::renderDataTable({
      btns <- ds_all() |> select(datasetId) |> 
        purrr::pmap_df(~ list(
          "操作" = as.character(a(href = route_link(paste0("dataset/show?datasetId=", .x)), "预览"))))
      ds_all() |> cbind(btns) |> semantic_DT(escape = F, selection = "none")
    })
  })
}