library(shiny)
library(glimmer)

ui <- fluidPage(
  funcEditorUI("func_editor")
)

server <- function(input, output, session) {
  funcEditorServer("func_editor")
}

shinyApp(ui, server)