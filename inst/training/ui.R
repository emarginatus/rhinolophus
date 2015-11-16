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
          value = 0.5,
          min = 0.05,
          max = 3,
          step = 0.05
        ),
        sliderInput(
          inputId = "amplitude",
          label = "Selection amplitude (rel. dB)",
          min = -10,
          max = 50,
          value = c(10, 20),
          step = 1
        ),
        width = 3
      ),
      mainPanel = mainPanel(
        plotOutput("plot", height = "700px"),
        width = 9
      ),
      position = "right"
    )
  )
)
