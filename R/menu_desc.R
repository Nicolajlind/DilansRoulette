menu_desc <- function(nummer){
  menukort <- dilans_menu()
  menu_entry <- menukort |> dplyr::filter(nummer == !!nummer)
  if(is.na(menu_entry$beskrivelse)){
    tekst <- paste0("NO: ", menu_entry$nummer, ": ", menu_entry$kategori, " - ", menu_entry$navn)
  } else {
    tekst <- paste0("NO: ", menu_entry$nummer, ": ", menu_entry$kategori, " - ", menu_entry$navn, " - ", menu_entry$beskrivelse)
  }
  tekst
}
