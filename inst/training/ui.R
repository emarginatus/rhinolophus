library(shiny)
library(rhinolophus)
shinyUI(
  fluidPage(
    titlePanel("Add training information"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        radioButtons(
          inputId = "channel",
          label = "Channel",
          choices = c("left", "right"),
          selected = "left",
          inline = TRUE,
          width = "100%"
        ),
        numericInput(
          inputId = "te.factor",
          label = "Time expansion factor",
          value = 10,
          min = 1,
          max = 100,
          step = 1,
          width = "100%"
        ),
        fileInput(
          inputId = "wav.file",
          label = "Upload a wav file",
          accept = "audio/x-wav",
          width = "100%"
        ),
        radioButtons(
          inputId = "window",
          label = "Moving window (ms)",
          choices = c("0.5", "1", "2"),
          selected = "0.5",
          inline = TRUE,
          width = "100%"
        ),
        sliderInput(
          inputId = "amplitude",
          label = "Selection amplitude (rel. dB)",
          min = -10,
          max = 50,
          value = c(10, 20),
          step = 1
        ),
        selectInput(
          inputId = "pulse",
          label = "Select a pulse",
          choices = ""
        ),
        sliderInput(
          inputId = "frequency",
          label = "Display frequency (kHz)",
          min = 0,
          max = 200,
          value = c(10, 150),
          step = 1
        ),
        sliderInput(
          inputId = "time",
          label = "Display time (ms)",
          min = 0,
          max = 200,
          value = c(0, 200),
          step = 1
        ),
        width = 3
      ),
      mainPanel = mainPanel(
        plotOutput("plot", height = "700px"),
        width = 9
      ),
      position = "left"
    )
  )
)
