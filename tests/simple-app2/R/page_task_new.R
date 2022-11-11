page_task_new_ui <- function(id) {
  ns <- NS(id)
  semanticPage(
    segment(
      class = "basic",
      titlePanel("new page"),
    )
  )
}
