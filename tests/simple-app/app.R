library(shiny)
library(glimmer)

setwd("../../")
source("tests/testthat/helper.R")
sample_init()

task_create(taskId = "demo-task") |>
  task_item_add(type = "func", script = "ds_demo", params = list(demoDataset = "mtcars")) |>
  task_item_add(type = "func", script = "ds_head", params = list(n = 20)) |>
  task_item_add(type = "func", script = "ds_arrange", params = list(columns = "disp", desc = TRUE))

ui <- fluidPage(
  sm_task_ui("task")
)

server <- function(input, output, session) {
  sm_task_server("task", taskId = "demo-task")
}

shinyApp(ui, server)