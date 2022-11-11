page_dataset_show_ui <- function(id) {
  ns <- NS(id)
  semanticPage(
    titlePanel(textOutput(ns("show-datasetName"))),
    segment(
      class = "basic",
      mod_preview_ui(ns("detail"))
    )
  )
}
page_dataset_show_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #
    dsNameRv <- reactive({
      dsId <-shiny.router::get_query_param("datasetId")
      if(!is.null(dsId)) {
        all <- ds_all()
        all$name[[all$datasetId |> purrr::detect_index(~ .x == dsId)]]
      } else {
        "-"
      }
    })
    #
    output$`show-datasetName` <- renderText(paste("数据集:", dsNameRv()))
    #
    observe({
      if(dsNameRv() != "-") {
        d <- ds_read0(dsNameRv()) |>
          collect() |>
          select(-contains("@"))
        mod_preview_server("detail", d)
      }
    })
  })
}