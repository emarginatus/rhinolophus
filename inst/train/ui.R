shinyUI(fluidPage(
  titlePanel("Add training data"),
  sidebarLayout(
    position = "right",
    sidebarPanel(
      actionButton("next.nosave", label = "Other pulse"),
      actionButton("next.save", label = "Save pulse"),
      uiOutput("i"),
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
