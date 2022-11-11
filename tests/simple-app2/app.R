library(shiny)
library(shiny.router)
library(shiny.semantic)
library(tibble)
library(tidyr)
library(dplyr)
library(plotly)
library(glimmer)

glimmer::config_load("./")
import_init()
risk_data_init()
task_queue_init()

## compenent ----

### preview ----
mod_preview_ui <- function(id, type = "tibble") {
  ns <- NS(id)
  switch(
    type,
    "tibble" = semantic_DTOutput(ns("DT")),
    textOutput(ns("unknown")))
}
#
mod_preview_server <- function(id, data, type = "tibble") {
  moduleServer(id, function(input, output, session) {
    switch(
      type,
      "tibble" = {
        output$DT <- DT::renderDataTable({
          data |> semantic_DT()
        })}
    )
  })
}

## page ----

### page_home ----
page_home_ui <- function(id) {
  ns <- NS(id)
  semanticPage(
    titlePanel("Home page"),
    p("This is the home page!")
  )
}

### page_import ----
page_import_ui <- function(id) {
  ns <- NS(id)
  semanticPage(
    titlePanel("所有导入素材"),
    segment(
      class = "basic",
      dropdown_input(
        ns("fileMatch"),
        choices = c(
          ".*",
          "^渣土工程/违规处置/违规报警",
          "^渣土工程/档案管理/渣车档案",
          "^渣土工程/档案管理/运输单位档案",
          "^渣土工程/档案管理/消纳单位档案",
          "^渣土工程/档案管理/施工单位档案",
          "^渣土工程/档案管理/工地信息档案",
          "^渣土工程/办证审批/排放证/排放证查看",
          "^渣土工程/办证审批/排放证/办证进度查询",
          "^渣土工程/办证审批/排放证/排放证发放流程环节",
          "^渣土工程/办证审批/排放证/排放证发放查看详情_v2",
          "^渣土工程/办证审批/运输证/运输证查看",
          "^渣土工程/办证审批/运输证/办证进度查询",
          "^渣土工程/办证审批/运输证/运输证发放流程环节",
          "^渣土工程/办证审批/运输证/运输证发放查看详情_v2"),
        value = ".*",
        type = "search selection fluid"),
      mod_preview_ui(ns("files"))
    ))
}
#
page_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    filesVal <- reactiveVal(NULL)
    observe({
      message(input$fileMatch)
      import_search(fileMatch = input$fileMatch, todoFlag = c(T, F)) |>
        select(-contains("@")) |>
        mutate(fileSize = as.integer(fileSize / 1024)) |>
        rename(`fileSize(K)` = fileSize) |>
        filesVal()
    })
    observe({
      if(!is.null(filesVal())) {
        mod_preview_server("files", filesVal())
      }
    })
  })
}

### page_datasets ----
page_datasets_ui <- function(id) {
  ns <- NS(id)
  semanticPage(
    titlePanel("所有数据集"),
    segment(
      class = "basic",
      semantic_DTOutput(ns("dataset-table"))
    )
  )
}
page_datasets_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    datasetToPreview <- reactiveVal(NULL)
    output$`dataset-table` <- DT::renderDataTable({
      btns <- ds_all() |> select(datasetId) |> 
        purrr::pmap_df(~ list(
          "操作" = as.character(a(href = route_link(paste0("dataset/show?datasetId=", .x)), "预览"))))
      ds_all() |> cbind(btns) |> semantic_DT(escape = F, selection = "none")
    })
  })
}

### page_dataset_show ----
page_dataset_show_ui <- function(id) {
  ns <- NS(id)
  semanticPage(
    titlePanel(textOutput(ns("show-datasetName"))),
    segment(
      class = "basic",
      semantic_DTOutput(ns("dataset-preview"))
    )
  )
}
page_dataset_show_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    dsNameRv <- reactive({
      dsId <-shiny.router::get_query_param("datasetId")
      if(!is.null(dsId)) {
        all <- ds_all()
        all$name[[all$datasetId |> purrr::detect_index(~ .x == dsId)]]
      } else {
        "-"
      }
    })
    output$`show-datasetName` <- renderText(paste("数据集:", dsNameRv()))
    output$`dataset-preview` <- DT::renderDataTable({
      if(dsNameRv() != "-") {
        ds_read0(dsNameRv()) |>
          collect() |>
          select(-contains("@")) |>
          semantic_DT()
      }
    })
  })
}
  

### page_tasks ----
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

### page_task_show ----
page_task_show_ui <- function(id) {
  ns <- NS(id)
  semanticPage(
    titlePanel("show page"),
    segment(
      class = "basic",
      textOutput(ns("show-taskId")),
      action_button(ns("show-to-edit"), "toEdit")
    )
  )
}
page_task_show_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$`show-taskId` <- renderText(
      shiny.router::get_query_param("taskId")
    )
    observeEvent(input$`show-to-edit`, {
      taskId <- shiny.router::get_query_param("taskId")
      shiny.router::change_page(paste0("task/edit?taskId=", taskId))
    })
  })
}

### page_task_edit ----
page_task_edit_ui <- function(id) {
  ns <- NS(id)
  semanticPage(
    titlePanel("edit page"),
    segment(
      class = "basic",
      textOutput(ns("edit-taskId"))
    )
  )
}
page_task_edit_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$`edit-taskId` <- renderText(
      shiny.router::get_query_param("taskId")
    )
  })
}

### page_task_new ----
page_task_new_ui <- function(id) {
  ns <- NS(id)
  semanticPage(
    segment(
      class = "basic",
      titlePanel("new page"),
    )
  )
}


## router ----
my_router <- function(urlName, nameFix, serverModule = TRUE) {
  uiName <- do.call(paste0("page_", nameFix, "_ui"), list(nameFix))
  if(serverModule) {
    serverName <- function(...) do.call(paste0("page_", nameFix, "_server"), list(nameFix))
  } else {
    serverName <- NA
  }
  route(urlName, uiName, serverName)
}
router <- make_router(
  my_router("/", "home", F),
  my_router("import", "import"),
  my_router("datasets", "datasets"),
  my_router("dataset/show", "dataset_show"),
  my_router("tasks", "tasks", F),
  my_router("task/show", "task_show"),
  my_router("task/edit", "task_edit"),
  my_router("task/new", "task_new", F)
)

## menu ----
menu <- shiny.semantic::horizontal_menu(
  list(
    list(name = "首页", link = route_link("/")),
    list(name = "导入素材", link = route_link("import")),
    list(name = "数据成果", link = route_link("datasets")),
    list(name = "分析任务", link = route_link("tasks")),
    list(name = "新的分析", link = route_link("task/new"))),
  logo = "vis.png"
)

## shiny ----
ui <- function() {
  fluidPage(
    menu,
    router$ui
  )
}

##
server <- function(input, output, session) {
  router$server(input, output, session)
}

##
shinyApp(ui, server)
