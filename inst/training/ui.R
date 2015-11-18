library(shiny)
library(rhinolophus)
shinyUI(
  fluidPage(
    fluidRow(
      column(
        2,
        wellPanel(
          fileInput(
            inputId = "wav.file",
            label = "Upload a wav file",
            accept = "audio/x-wav",
            width = "100%"
          ),
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
          )
        ),
        wellPanel(
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
          )
        ),
        wellPanel(
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
          )
        )
      ),
      column(
        2,
        selectInput(
          inputId = "pulse",
          label = "",
          choices = c("Select a pulse" = "")
        ),
        radioButtons(
          inputId = "species",
          label = "Choose species",
          choices = c("noise"),
          inline = TRUE,
          width = "100%"
        ),
        textInput(
          inputId = "new.species",
          label = "Create new species",
          width = "100%"
        ),
        radioButtons(
          inputId = "type",
          label = "Choose call type",
          choices = "call",
          inline = TRUE,
          width = "100%"
        ),
        textInput(
          inputId = "new.type",
          label = "Create new call type",
          width = "100%"
        ),
        actionButton(
          inputId = "new.truth",
          label = "Confirm classification"
        )
      ),
      column(
        8,
        plotOutput("plot", height = "700px")
      )
    )
  )
)
