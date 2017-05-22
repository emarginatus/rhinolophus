library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Sonogram"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput(
        "path",
        label = "path",
        value = "."
      ),
      sliderInput(
        "frequency",
        label = "frequency (kHz)",
        value = c(20, 60),
        min = 0,
        max = 150
      ),
      sliderInput(
        "amplitude",
        label = "min amplitude (dB)",
        value = 10,
        min = -50,
        max = 50
      )
    ),

    mainPanel(
       plotOutput("sonogram", height = "900px")
    )
  )
))
