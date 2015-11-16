library(shiny)
library(rhinolophus)
shinyUI(
  fluidPage(
    titlePanel("Add training information"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        fileInput(
          inputId = "pulse.file",
          label = "Upload a batPulse object",
          width = "100px"
        ),
        numericInput(
         "i",
          label = "Pulse",
          value = 1,
         width = "100px"
        )
      ),
      mainPanel = mainPanel(
        textOutput("filename"),
        plotOutput("plot.pulse")
      ),
      position = "left"
    )
  )
)
