#' @title UI af dashboardet
ui_func <- function() {
  shinydashboardPlus::dashboardPage(
    header = shinydashboardPlus::dashboardHeader(),
    sidebar = shinydashboardPlus::dashboardSidebar(disable = TRUE),  
    body = shinydashboard::dashboardBody(
      shinyjs::useShinyjs(),
      shiny::fluidRow(
        shinydashboardPlus::box(
          title = "Dilans Roulette",
          width = 12,
          shiny::textInput(
            inputId = "navne",
            label = "Indskriv navne - sepereret med komma",
            placeholder = "F.eks. 'SA, Noah, Ernst'"
          ),
          shiny::actionButton(
            inputId = "navneDone",
            label = "Go!"
          ),
          shiny::conditionalPanel(
            condition = "input.navneDone > 0",
            shiny::fluidRow(
              shinydashboardPlus::box(
                title = "Deltagere",
                width = 6,
                reactable::reactableOutput(outputId = "deltagere")
              )
            ),
            shiny::textOutput(outputId = "current_player"),
            shiny::actionButton(inputId = "roll", label = "Roll!"),
            shiny::actionButton(inputId = "keep", label = "Vælg"),
            shiny::actionButton(inputId = "reroll", label = "Reroll!")
          )
        )
      )
    )
  )
}