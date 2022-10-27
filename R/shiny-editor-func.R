#' @export
ui_func_editor <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      radioButtons(ns("funs_topic"),
                  label = "选择函数域",
                  selected = "ds",
                  choices = get_funs_schema()$items),
      uiOutput(ns("func_panel")),
      uiOutput(ns("params_panel")),
      hr(style = "background-color:#ababab; height:1px; border:none;"),
      actionButton(ns("run"), label = "执行", class = "btn btn-primary")
    ),
    mainPanel(
      uiOutput(ns("preview"))
    )
  )
}

#' @export
server_func_editor <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    output$func_panel <- renderUI({
      selectInput(
        ns("func_name"),
        label = "选择函数名称", 
        choices = get_funs_schema(input$funs_topic, "functions")$items
        )
    })
    output$params_panel <- renderUI({
      message("New Func: ", input$func_name)
      if(!is.null(input$func_name)) {
        ## 提取默认参数
        x <- Filter(
          function(j) !is.null(j),
          formals("ds_read0") |>
            purrr::map(function(i) if(class(i) == "name") NULL else i))
        ## 获得参数元数据
        items <- get_funs_schema(input$funs_topic, "functions", input$func_name, "params")$items
        message("params: ", items |> paste(collapse = ", "))
        items |> purrr::map(function(i) {
          p <- get_funs_schema(input$funs_topic, "functions", input$func_name, "params", i)
          inputParamId <- paste0(input$func_name, "$", i)
          ## 动态创建参数编辑组件
          if(!is.null(p$editType) && p$editType %in% c("dataset_search")) {
            selectInput(ns(inputParamId), label = i, selected = x[[i]], choices = ds_all()$name)
          } else {
            ## 找不到合适的输入组件
            actionButton(ns(inputParamId), label = i)
          }
        })
      }
    })

    ##
    observeEvent(input$run, {
      ## 提取参数值
      myparams <- list()
      items <- get_funs_schema(input$funs_topic, "functions", input$func_name, "params")$items
      message("params: ", myparams |> paste(collapse = ", "))
      items |> purrr::map(function(i) {
        myparams[[i]] <<- input[[paste0(input$func_name, "$", i)]]
      })
      ## 执行函数
      message("Func >> ", input$func_name,  " / : <", myparams, ">")
      resp <- do.call(input$func_name, args = myparams)
      print(resp)
      if("data.frame" %in% class(resp)) {
        output$preview <- renderUI(DT::DTOutput(ns("dt_preview")))
        output$dt_preview <- DT::renderDT({
          resp |> DT::datatable()
        })
      }
    }, ignoreInit = TRUE)
  })
}