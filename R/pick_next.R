pick_next <- function(current_id, game_info){
  filtered_game_info <- game_info |>
    dplyr::filter(valgt == "")

  possible_id <- filtered_game_info |>
    dplyr::filter(current_id < ID) |>
    dplyr::pull(ID) |>
    minimum()

  if(is.null(possible_id)){
    possible_id <- filtered_game_info |>
      dplyr::pull(ID) |>
      minimum()
  }
  return(possible_id)
}
