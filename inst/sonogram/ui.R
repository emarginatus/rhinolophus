library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Sonogram"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      width = 2,
      textInput(
        "path",
        label = "path",
        value = "."
      ),
      actionButton("update_path", label = "load file"),
      actionButton("step_backward", label = "<<<"),
      actionButton("step_forward", label = ">>>"),
      sliderInput(
        "frequency",
        label = "frequency (kHz)",
        value = c(0, 150),
        min = 0,
        max = 150
      ),
      sliderInput(
        "starttime",
        label = "start time (ms)",
        value = 0,
        min = 0,
        max = 10000
      ),
      sliderInput(
        "timeinterval",
        label = "interval (ms)",
        value = 200,
        min = 50,
        max = 1000
      ),
      sliderInput(
        "amplitude",
        label = "amplitude (dB)",
        value = c(0, 50),
        min = -50,
        max = 100
      )
    ),

    mainPanel(
       plotOutput("sonogram", height = "900px")
    )
  )
))
