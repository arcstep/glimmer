page_bucket_show_ui <- function(id) {
  ns <- NS(id)
  semanticPage(
    titlePanel("show bucket page"),
    segment(
    )
  )
}
page_bucket_show_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}
