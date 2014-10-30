shinyUI(fluidPage(
  titlePanel("Add training data"),
  sidebarLayout(
    position = "right",
    sidebarPanel(
      actionButton("next.nosave", label = "Other pulse"),
      numericInput(
        "i",
        label = "Pulse",
        value = sample.pulse(predictions)
      ),
      uiOutput("amplitude"),
      uiOutput("time"),
      uiOutput("frequency"),
      uiOutput("types"),
      uiOutput("species")
    ),
    mainPanel(
      plotOutput("pulsePlot")
    )
  )
))
