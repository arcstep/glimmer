library(shiny)
library(glimmer)

config_init("./")
import_init()
risk_data_init()
task_queue_init()

task_create(taskId = "demo-task1", force = TRUE) |>
  script_var_add(globalVars = list("myds" = mtcars)) |>
  # script_item_add(type = "func", script = "ds_demo", params = list(demoDataset = "mtcars")) |>
  script_item_add(type = "func", script = "ds_head", params = list(n = 20), inputAssign = list(d = "myds")) |>
  script_item_add(type = "func", script = "ds_arrange", outputAssign = "x", params = list(columns = "disp", desc = TRUE))

task_create(taskId = "demo-task2", force = TRUE) |>
  script_var_add() |>
  script_item_add(type = "func", script = "ds_demo", params = list(demoDataset = "mpg")) |>
  script_item_add(type = "func", script = "ds_arrange", params = list(columns = "displ", desc = TRUE))

task_create(taskId = "demo-task3", force = TRUE) |>
  script_item_add(type = "func", script = "ds_demo", params = list(demoDataset = "mpg")) |>
  script_item_add(type = "func", script = "ds_count", params = list(columns = "model", sort = TRUE, name = "车型数量"))

######
# source("~/R-workspace/glimmer/R/sm_task.R")
# source("~/R-workspace/glimmer/R/sm_scripts.R")
# source("~/R-workspace/glimmer/R/sm_params.R")
# source("~/R-workspace/glimmer/R/sm_preview.R")
# source("~/R-workspace/glimmer/R/sm_global_vars.R")
# source("~/R-workspace/glimmer/R/sm_input_assign.R")
# source("~/R-workspace/glimmer/R/sm_output_assign.R")

ui <- fluidPage(
  fluidRow(
    column(6,
           actionLink("task1", label = "Demo1", style = "margin: 5px"),
           actionLink("task2", label = "Demo2", style = "margin: 5px")),
    column(6,
           actionButton("import-btn", label = "导入", class = "btn-xs", style = "margin: 5px"),
           actionButton("tasks-btn", label = "数据处理", class = "btn-xs", style = "margin: 5px"),
           actionButton("dataset-btn", label = "数据集", class = "btn-xs",  style = "margin: 5px"),
           style = "text-align: right;"),
    style="background-color: #4b8a3b1c;"
  ),
  fluidRow(sm_task_ui("task"))
)

server <- function(input, output, session) {
  choosedTaskIdVal <- reactiveVal("demo-task1")
  
  ## 菜单
  observeEvent(input$task1, { choosedTaskIdVal("demo-task1") })
  observeEvent(input$task2, { choosedTaskIdVal("demo-task2") })
  
  ## 选择任务
  taskModal <- function(failed = FALSE) {
    modalDialog(
      title = "选择要执行的任务",
      tags$div(
        selectInput("choose-task-id",
                    label = "任务列表",
                    choices = task_search()$taskId)
        ),
      if (failed)
        div(tags$b("没有找到可以执行的任务，请重新选择...", style = "color: red;")),
      
      footer = tagList(
        modalButton("取消"),
        actionButton("chooseTask", "确定")
      )
    )
  }
  observeEvent(input$chooseTask, {
    if(task_exists(input$`choose-task-id`)) {
      message("CHOOSE: ", input$`choose-task-id`)
      choosedTaskIdVal(input$`choose-task-id`)
      removeModal()
    } else {
      showModal(taskModal(failed = TRUE))
    }
  })
  observeEvent(input$`tasks-btn`, {
    showModal(taskModal())
  })
  ## 选择导入任务
  
  ## 选择数据集查看任务
  
  ## 选择taskId
  observe({ sm_task_server("task", choosedTaskIdVal()) })
}

shinyApp(ui, server)


