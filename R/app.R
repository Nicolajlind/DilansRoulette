#' @title Kørsel af dashboardet
#' @export
run_app <- function(){
  shiny::shinyApp(
    ui = ui_func,
    server = server_func
  )
}