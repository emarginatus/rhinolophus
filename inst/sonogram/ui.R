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
      ),
      selectInput(
        "channel",
        label = "channel + TE factor",
        choices = c("left, TE = 1", "right, TE = 10")
      ),
      sliderInput(
        "starttime",
        label = "start time (ms)",
        value = 0,
        min = 0,
        max = 10000,
        animate = animationOptions(interval = 500)
      ),
      actionButton("move_file", label = "move file"),
      checkboxGroupInput("species", label = "species", inline = TRUE),
      textInput("new_species", label = "new species"),
      actionButton("add_species", label = "add new species")
    ),

    mainPanel(
       plotOutput("sonogram", height = "900px")
    )
  )
))
