library(shiny)

shinyUI(fluidPage(

  titlePanel("Add reference pulses"),

  sidebarLayout(
    sidebarPanel(
      textInput(
        "path",
        label = "Path",
        value = "/media/thierry_onkelinx/CE74C8F474C8E077/vleermuizen/opnames"
      ),
      actionButton(
        inputId = "new.path",
        label = "Read files"
      ),
      radioButtons(
        inputId = "plot_type",
        label = "Select the image type",
        choices = c("spectrogram", "pulse"),
        selected = "spectrogram",
        inline = TRUE,
        width = "50%"
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
        choices = "C",
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

    mainPanel(
      plotOutput("plot", height = "800px")
    )
  )
))
