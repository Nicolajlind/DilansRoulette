dilans_menu <- function(){
  file <- system.file("dilansmenu.yml", package = "DilansRoulette")
  yaml::read_yaml(file = file) |>
    dplyr::bind_rows()
}