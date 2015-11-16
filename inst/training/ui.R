library(shiny)
library(rhinolophus)
shinyUI(
  fluidPage(
    titlePanel("Add training information"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        fileInput(
          inputId = "wav.file",
          label = "Upload a batWav object",
          width = "100px"
        ),
        numericInput(
         "i",
          label = "Pulse",
          value = 1,
         width = "100px"
        ),
        width = 2
      ),
      mainPanel = mainPanel(
        plotOutput("plot", height = "700px")
      ),
      position = "right"
    )
  )
)
