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
        sliderInput(
          inputId = "window",
          label = "Moving window (ms)",
          value = 1,
          min = 0.1,
          max = 10,
          step = 0.1
        ),
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
