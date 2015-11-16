library(shiny)
library(rhinolophus)
library(dplyr)

shinyServer(
  function(input, output){
    wav <- reactive({
      if (is.null(input$wav.file)) {
        return(
          list()
        )
      }
      import.env <- new.env()
      load(input$wav.file$datapath, envir = import.env)
      get("wav", envir = import.env)
    })

    output$filename <- renderText({
      if (is.null(input$wav.file)) {
        return(character(0)        )
      }
      wav()@Metadata$Filename
    })

    spectrogram <- reactive({
      if (is.null(input$wav.file)) {
        return(
          list()
        )
      }
      wav2spectrogram(wav(), window.ms = 1)
    })

    spectrogram.raster <- reactive({
      if (is.null(input$wav.file)) {
        return(character(0)        )
      }
      spectrogram_raster(spectrogram = spectrogram()@Spectrogram[[1]])
    })

    pulses <- reactive({
      find_pulses(
        spectrogram = spectrogram(),
        min.contour = 10,
        min.peak = 20
      )
    })

    borders <- reactive({
      pulse_border(pulses())
    })

    output$plot <- renderPlot({
      if (is.null(input$wav.file)) {
        return(character(0))
      }
      plot(spectrogram.raster(), asp = 0.5e-6)
      lines(borders())
    })
  }
)
