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

## page ----

### home_page ----
home_page <- semanticPage(
  titlePanel("Home page"),
  p("This is the home page!")
)

### import_page ----
import_page <- semanticPage(
  titlePanel("所有导入素材"),
  segment(
    class = "basic",
    dropdown_input(
      "import-fileMatch",
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
    semantic_DTOutput("import-table")
  )
)
import_page_callback <- function(input, output, session) {
  output$`import-table` <- DT::renderDataTable(
    import_search(fileMatch = input$`import-fileMatch`,
                  todoFlag = c(T, F)) |>
      select(-contains("@")) |>
      mutate(fileSize = as.integer(fileSize / 1024)) |>
      rename(`fileSize(Kb)` = fileSize) |>
      semantic_DT()
  )
}

### datasets_page ----
datasets_page <- semanticPage(
  titlePanel("所有数据集"),
  segment(
    class = "basic",
    semantic_DTOutput("dataset-table")
  )
)
datasets_page_callback <- function(input, output, session) {
  datasetToPreview <- reactiveVal(NULL)
  output$`dataset-table` <- DT::renderDataTable({
    btns <- ds_all() |> select(datasetId) |> 
      purrr::pmap_df(~ list(
        "操作" = as.character(a(href = route_link(paste0("dataset/show?datasetId=", .x)), "预览"))))
    ds_all() |> cbind(btns) |> semantic_DT(escape = F, selection = "none")
  })
}

### dataset_show_page ----
dataset_show_page <- semanticPage(
  titlePanel(textOutput("show-datasetName")),
  segment(
    class = "basic",
    semantic_DTOutput("dataset-preview")
  )
)
dataset_show_page_callback <- function(input, output, session) {
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
}

### tasks_page ----
tasks_page <- semanticPage(
  titlePanel("tasks page"),
  segment(
    class = "basic",
    actionLink("BC", a(href = route_link("task/show?taskId=BC"), "BC")),
    p(),
    actionLink("AA", a(href = route_link("task/edit?taskId=AA"), "AA"))
  )
)

### task_show_page ----
task_show_page <- semanticPage(
  titlePanel("show page"),
  segment(
    class = "basic",
    textOutput("show-taskId"),
    action_button("show-to-edit", "toEdit")
  )
)
task_show_page_callback <- function(input, output, session) {
  output$`show-taskId` <- renderText(
    shiny.router::get_query_param("taskId")
  )
  observeEvent(input$`show-to-edit`, {
    taskId <- shiny.router::get_query_param("taskId")
    shiny.router::change_page(paste0("task/edit?taskId=", taskId))
  })
}

### task_edit_page ----
task_edit_page <- semanticPage(
  titlePanel("edit page"),
  segment(
    class = "basic",
    textOutput("edit-taskId")
  )
)
task_edit_page_callback <- function(input, output, session) {
  output$`edit-taskId` <- renderText(
    shiny.router::get_query_param("taskId")
  )
}

### task_new_page ----
task_new_page <- semanticPage(
  segment(
    class = "basic",
    titlePanel("new page"),
  )
)

## router ----
router <- make_router(
  route("/", home_page, NA),
  route("import", import_page, import_page_callback),
  route("datasets", datasets_page, datasets_page_callback),
  route("dataset/show", dataset_show_page, dataset_show_page_callback),
  route("tasks", tasks_page, NA),
  route("task/show", task_show_page, task_show_page_callback),
  route("task/edit", task_edit_page, task_edit_page_callback),
  route("task/new", task_new_page, NA)
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
