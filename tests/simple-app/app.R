library(shiny)
library(glimmer)

setwd("../../")
source("tests/testthat/helper.R")
sample_init()

print(task_search())

ui <- fluidPage(
  sm_ui_task("task")
)

server <- function(input, output, session) {
  sm_server_task("task", taskId = "task_sample_simple")
}

shinyApp(ui, server)