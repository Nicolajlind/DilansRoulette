#' @title Serverdelen af dashboardet
server_func <- function(input, output) {
  react_list_names <- shiny::reactiveVal(value = NULL)
  react_game_data <- shiny::reactiveVal(value = NULL)
  react_current_menu_item <- shiny::reactiveVal(value = NULL)

  menukort <- dilans_menu()

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

  output$rolled_item <- shiny::renderText({
    shiny::req(react_current_menu_item())
    item <- react_current_menu_item()
    paste0("Der er rullet: ", item)
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

  react_next_player_id <- shiny::reactive({
    shiny::req(react_game_data())
    antal_spins <- input$reroll + input$keep
    active_ids <- react_game_data() |>
      dplyr::filter(valgt == "") |> dplyr::pull(ID)

    next_id <- 1 + antal_spins %% length(active_ids)
    active_ids[next_id]
  })

  react_current_player_id <- shiny::reactive({
    shiny::req(react_game_data())
    antal_spins <- input$reroll + input$keep - 1
    active_ids <- react_game_data() |>
      dplyr::filter(valgt == "") |> dplyr::pull(ID)

    next_id <- 1 + antal_spins %% length(active_ids)
    active_ids[next_id]
  })

  output$current_player <- shiny::renderText({
    shiny::req(react_next_player_id())
    id <- react_next_player_id()
    spiller <- react_game_data() |>
      dplyr::filter(ID == id) |>
      dplyr::pull("person")

    paste0("Nuværende spiller er: ", spiller)
  })

  shiny::observeEvent(input$roll, {
    shinyjs::hide(id = "roll")

    rolled_no <- sample(menukort$nummer, size = 1)
    menu_entry <- menukort |> dplyr::filter(nummer == rolled_no)
    tekst <- paste0("NO: ", menu_entry$nummer, ": ", menu_entry$navn, " - ", menu_entry$beskrivelse)
    react_current_menu_item(tekst)
    shinyjs::show(id = "rolled_item")
    shinyjs::show(id = "reroll")
    shinyjs::show(id = "keep")
  })

  shiny::observeEvent(input$keep, {
    shiny::req(react_game_data())
    shiny::req(react_current_player_id())
    shiny::req(react_current_menu_item())
    shinyjs::hide(id = "rolled_item")
    shinyjs::hide(id = "reroll")
    shinyjs::hide(id = "keep")

    spiller_id <- react_current_player_id()
    .data <- react_game_data()
    .data$forsog[.data$ID == spiller_id] <- 1 + .data$forsog[.data$ID == spiller_id]
    .data$valgt[.data$ID == spiller_id] <- react_current_menu_item()
    react_game_data(.data)
    shiny::req(react_next_player_id())
    shinyjs::show(id = "roll")
  })

  shiny::observeEvent(input$reroll, {
    shiny::req(react_game_data())
    shiny::req(react_current_player_id())
    shinyjs::hide(id = "rolled_item")
    shinyjs::hide(id = "reroll")
    shinyjs::hide(id = "keep")

    spiller_id <- react_current_player_id()
    .data <- react_game_data()
    .data$forsog[.data$ID == spiller_id] <- 1 + .data$forsog[.data$ID == spiller_id]
    react_game_data(.data)
    shiny::req(react_next_player_id())
    shinyjs::show(id = "roll")
  })

}
