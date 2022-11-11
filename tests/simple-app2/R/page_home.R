page_home_ui <- function(id) {
  ns <- NS(id)
  semanticPage(
    titlePanel("Home page"),
    p("This is the home page!")
  )
}
