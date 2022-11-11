mod_preview_ui <- function(id, type = "tibble") {
  ns <- NS(id)
  switch(
    type,
    "tibble" = mod_preview_DT_ui(ns("tibble")),
    textOutput(ns("unknown"))
  )
}
#
mod_preview_server <- function(id, data, type = "tibble") {
  moduleServer(id, function(input, output, session) {
    switch(
      type,
      "tibble" = mod_preview_DT_server("tibble", data),
      output$unknown <- shiny::renderText(
        paste0("< ", paste(class(data) |> unlist(), collapse = ", "), " >"))
    )
  })
}