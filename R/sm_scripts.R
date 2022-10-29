#' @export
sm_scripts_ui <- function(id, scriptItems) {
  message("sm_scripts_ui:", id)
  ns <- NS(id)
  ##
  shiny::tags$ol(
    !!!(scriptItems |> purrr::pmap(function(rowid, type, script, params, globalVars, inputAsign, outputAsign) {
      shiny::tags$li(
        span(paste0(type)),
        br(),
        shiny::tags$code(script |> as.character()),
        sm_params_ui(ns(rowid), params),
        p()
      )
    }))
  )
}

#' @export
sm_scripts_server <- function(id, scriptItems) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    message("sm_scripts_server id:", id)
  })
}
