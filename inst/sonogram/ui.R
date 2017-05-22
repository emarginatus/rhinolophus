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
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("sonogram")
    )
  )
))
