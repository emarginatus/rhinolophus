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

    spectrogram <- reactive({
      if (is.null(input$wav.file)) {
        return(
          list()
        )
      }
      wav2spectrogram(wav(), window.ms = as.numeric(input$window))
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
        min.contour = as.numeric(input$amplitude[1]),
        min.peak = as.numeric(input$amplitude[2])
      )
    })

    borders <- reactive({
      pulse_border(pulses())
    })

    output$plot <- renderPlot({
      if (is.null(input$wav.file)) {
        return(character(0))
      }
      plot(spectrogram.raster(), asp = 0.5e-6, main = wav()@Metadata$Filename, xlab = "Time (s)", ylab = "Frequency (Hz)")
      lines(borders())
    })
  }
)
