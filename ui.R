

fluidPage(
  fluidRow(
    plotlyOutput("carte_practiciens")
  ),
  fluidRow(
    align = "center",
    radioButtons(
      inputId = "choix_practicien",
      label = "Practiciens",
      choices = c("Gynécologues", "Opthalmologues", "Pédiatres"),
      selected = "Gynécologues",
      inline = TRUE
    )
  )
)