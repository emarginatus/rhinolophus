library(shiny)

shinyUI(fluidPage(

  titlePanel("Check the automatic dermination of pulses"),

  sidebarLayout(
    sidebarPanel(
      textInput(
        "path",
        label = "Path",
        value = "/media/thierry_onkelinx/CE74C8F474C8E077/vleermuizen/opnames/kesterbeek"
      ),
      actionButton(
        inputId = "new.path",
        label = "Read files"
      )
    ),

    mainPanel(
      verbatimTextOutput("filename"),
      plotOutput("plot")
    )
  )
))
