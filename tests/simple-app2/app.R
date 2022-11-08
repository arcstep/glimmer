library(shiny)
library(bslib)

light <- bs_theme()
dark <- bs_theme(bg = "black", fg = "white", primary = "purple")
ui <- fluidPage(
  theme = light, 
  checkboxInput("dark_mode", "Dark mode")
)
server <- function(input, output, session) {
  observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) dark else light
  ))
}
shinyApp(ui, server)