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
                width = 10,
                reactable::reactableOutput(outputId = "deltagere")
              )
            ),
            shiny::textOutput(outputId = "current_player"),
            shiny::textOutput(outputId = "rolled_item"),
            shiny::actionButton(inputId = "roll", label = "Roll!"),
            shiny::actionButton(inputId = "keep", label = "V\u00e6lg"),
            shiny::actionButton(inputId = "reroll", label = "Reroll!")
          )
        )
      )
    )
  )
}
