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
      bat.wav <- read_wav(
        input$wav.file$datapath,
        channel = input$channel,
        te.factor = input$te.factor
      )
      bat.wav@Metadata$Filename <- input$wav.file$name
      bat.wav
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

    ramp <- reactive({
      midpoint_ramp(
        spectrogram.raster(),
        mid.point = as.numeric(input$amplitude[1]),
        n = 20
      )
    })

    output$plot <- renderPlot({
      if (is.null(input$wav.file)) {
        return(character(0))
      }
      plot(
        spectrogram.raster(),
        asp = 0.5e-6,
        main = wav()@Metadata$Filename,
        breaks = ramp()$Breaks,
        col = ramp()$Colour,
        ylim = input$frequency * 1e3,
        xlab = "Time (s)",
        ylab = "Frequency (Hz)"
      )
      lines(borders())
    })
  }
)
