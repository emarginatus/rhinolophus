library(shiny)
library(dplyr)
library(raster)
library(rhinolophus)

shinyServer(function(input, output) {
  filename <- eventReactive(
    input$new.path, {
      if (is.null(input$path)) {
        return("")
      }
      list.files(
        input$path,
        pattern = "\\.rds$",
        full.names = TRUE,
        recursive = TRUE,
        ignore.case = TRUE
      ) %>%
        sample(1)
  })

  pulses <- reactive({
    if (filename() == "") {
      return(NULL)
    }
    readRDS(file = filename())
  })

  spectrogram <- reactive({
    if (is.null(pulses())) {
      return(NULL)
    }
    spectrogram_raster(pulses()@Spectrogram[[1]])
  })

  output$filename <- renderText(filename())

  output$plot <- renderPlot({
    if (is.null(pulses())) {
      return(character(0))
    }
    plot(
      spectrogram(),
      asp = 0.5
    )
  })
})
