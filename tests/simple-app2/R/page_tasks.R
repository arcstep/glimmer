page_tasks_ui <- function(id) {
  ns <- NS(id)
  semanticPage(
    titlePanel("tasks page"),
    segment(
      class = "basic",
      actionLink("BC", a(href = route_link("task/show?taskId=BC"), "BC")),
      p(),
      actionLink("AA", a(href = route_link("task/edit?taskId=AA"), "AA"))
    )
  )
}