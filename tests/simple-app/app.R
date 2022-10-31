library(shiny)
library(glimmer)

config_init("./")
import_init()
risk_data_init()
task_queue_init()

task_create(taskId = "demo-task1", force = TRUE) |>
  script_item_add(type = "func", script = "ds_demo", params = list(demoDataset = "mtcars")) |>
  script_item_add(type = "func", script = "ds_head", params = list(n = 20)) |>
  script_item_add(type = "func", script = "ds_arrange", params = list(columns = "disp", desc = TRUE))

task_create(taskId = "demo-task2", force = TRUE) |>
  script_item_add(type = "func", script = "ds_demo", params = list(demoDataset = "mpg")) |>
  script_item_add(type = "func", script = "ds_arrange", params = list(columns = "displ", desc = TRUE))

task_create(taskId = "demo-task3", force = TRUE) |>
  script_item_add(type = "func", script = "ds_demo", params = list(demoDataset = "mpg")) |>
  script_item_add(type = "func", script = "ds_count", params = list(columns = "model", sort = TRUE, name = "车型数量"))

######
source("~/R-workspace/glimmer/R/sm_task.R")
source("~/R-workspace/glimmer/R/sm_scripts.R")
source("~/R-workspace/glimmer/R/sm_params.R")
source("~/R-workspace/glimmer/R/sm_preview.R")

ui <- fluidPage(
  sm_task_ui("task")
)

server <- function(input, output, session) {
  sm_task_server("task", taskId = "demo-task")
}

shinyApp(ui, server)


