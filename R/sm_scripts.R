#' @title Shiny Modules UI - 任务脚本
#' @family Shiny Modules functions
#' @export
sm_scripts_ui <- function(id, scriptItems) {
  message("sm_scripts_ui:", id)
  ns <- NS(id)
  ##
  shiny::tags$div(
    !!!(scriptItems |> purrr::pmap(function(rowid, type, script, params, globalVars, inputAsign, outputAsign) {
      shiny::tags$div(
        span(paste0(type)),
        br(),
        shiny::tags$code(script |> as.character()),
        sm_params_ui(ns(rowid), params),
        p()
      )
    }))
  )
}

#' @title Shiny Modules Server - 任务脚本
#' @family Shiny Modules functions
#' @export
sm_scripts_server <- function(id, scriptItems) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    message("sm_scripts_server id:", id)
    scriptItems |>
      purrr::pwalk(function(rowid, type, script, params, globalVars, inputAsign, outputAsign) {
        sm_params_server(rowid, params)
      })
  })
}
