library(shiny)
library(shiny.router)
library(shiny.semantic)
library(tibble)
library(tidyr)
library(dplyr)
library(plotly)
library(glimmer)
library(sortable)

glimmer::config_load("./")
import_init()
risk_data_init()
task_queue_init()

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

##
router <- make_router(
  my_router("/", "home", F),
  my_router("import", "import"),
  my_router("datasets", "datasets"),
  my_router("dataset/show", "dataset_show"),
  my_router("bucket", "bucket", F),
  my_router("bucket/show", "bucket_show"),
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
    list(name = "数据篮", link = route_link("bucket"))
    # list(name = "新的分析", link = route_link("task/new"))
    )
)

## shiny ----
ui <- function() {
  fluidPage(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "global.css")),
    div(menu, style = "width: 500px"),
    router$ui
  )
}

##
server <- function(input, output, session) {
  bucket_set(session, "file", "a.csv")
  session$userData$myval <- "abcdefg"
  router$server(input, output, session)
}

##
shinyApp(ui, server)
