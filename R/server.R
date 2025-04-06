#' @title Serverdelen af dashboardet
server_func <- function(input, output) {
  react_list_names <- shiny::reactiveVal(value = NULL)
  react_game_data <- shiny::reactiveVal(value = NULL)
  
  output$deltagere <- reactable::renderReactable({
    shiny::req(react_game_data())
    .data <- react_game_data()
    reactable::reactable(
      .data,
      columns = list(
        person = reactable::colDef(name = "Navn"),
        forsog = reactable::colDef(name ="Antal rolls"),
        valgt = reactable::colDef(name = "Valgt")
      )
    )
  })
  
  shiny::observeEvent(input$navneDone, {
    names_vector <- strsplit(input$navne, ",\\s*")[[1]]
    .data <- data.frame(
      person = names_vector,
      forsog = rep(0, length(names_vector)),
      valgt = rep("", length(names_vector))
    ) |> dplyr::mutate(ID = dplyr::row_number(), .before = 1L)
    shinyjs::hide(id = "navne")
    shinyjs::hide(id = "navneDone")
    shinyjs::hide(id = "keep")
    shinyjs::hide(id = "reroll")
    shinyjs::show(id = "roll")
    react_list_names(names_vector)
    react_game_data(.data)
  })
  
  react_current_player_id <- shiny::reactive({
    shiny::req(react_list_names())
    antal_spins <- input$roll
    antal_spillere <- length(react_list_names())
    
    1 + antal_spins %% antal_spillere
  })
  
  output$current_player <- shiny::renderText({
    shiny::req(react_current_player_id())
    id <- react_current_player_id()
    spiller <- react_game_data() |> 
      dplyr::filter(ID == id) |>
      dplyr::pull("person")
    
    paste0("Nuværende spiller er: ", spiller)
  })
  
  shiny::observeEvent()

}