#' @title Shiny Modules UI - 任务脚本
#' @family Shiny Modules functions
#' @export
sm_scripts_ui <- function(id, scriptItems) {
  ns <- function(...) NS(id)(paste(..., sep = "-"))
  ##
  shiny::tags$div(
    !!!(scriptItems |> purrr::pmap(function(rowid, type, script, params, globalVars, inputAssign, outputAssign) {
      shiny::tags$div(
        span(paste0(type), " >> "),
        shiny::tags$code(script |> as.character()),
        sm_global_vars_ui(ns("global-vars", rowid), globalVars),
        sm_input_assign_ui(ns("input-assign", rowid), inputAssign),
        sm_output_assign_ui(ns("output-assign", rowid), unlist(outputAssign)),
        sm_params_ui(ns("params", rowid), params),
        p()
      )
    }))
  )
}

#' @title Shiny Modules Server - 任务脚本
#' @family Shiny Modules functions
#' @export
sm_scripts_server <- function(id, scriptItems) {
  moduleServer(id, function(input, output, session) {
    observe({
      scriptItems |>
        purrr::pwalk(function(rowid, type, script, params, globalVars, inputAssign, outputAssign) {
          sm_params_server(paste("params", rowid, sep = "-"), params)
          sm_global_vars_server(paste("global-vars", rowid, sep = "-"), globalVars)
          sm_input_assign_server(paste("input-assign", rowid, sep = "-"), unlist(inputAssign))
          sm_output_assign_server(paste("output-assign", rowid, sep = "-"), unlist(outputAssign))
        })
    })
  })
}
