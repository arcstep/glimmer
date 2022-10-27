library(shiny)
library(glimmer)

rootPath <- tempdir()
config_init(rootPath)
import_init()
risk_data_init()
task_queue_init()

ui <- fluidPage(
  titlePanel("任务函数"),
  ui_func_editor("func_editor")
)

server <- function(input, output, session) {
  server_func_editor("func_editor")
}

shinyApp(ui, server)