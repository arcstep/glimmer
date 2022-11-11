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