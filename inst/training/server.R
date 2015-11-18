library(shiny)
library(rhinolophus)
library(dplyr)

shinyServer(
  function(input, output, session){
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

    observe({
      if (is.null(input$wav.file)) {
        return(NULL)
      }
      frequency.range <- range(
        pretty(spectrogram()@Spectrogram[[1]]$f, 50)
      ) / 1e3
      updateSliderInput(
        session = session,
        inputId = "frequency",
        value = c(
          max(10, frequency.range[1]),
          min(150, frequency.range[2])
        ),
        min = frequency.range[1],
        max = frequency.range[2]
      )
      time.range <- range(
        pretty(spectrogram()@Spectrogram[[1]]$t, 50)
      ) * 1e3
      updateSliderInput(
        session = session,
        inputId = "time",
        value = c(
          max(0, time.range[1]),
          min(200, time.range[2])
        ),
        min = time.range[1],
        max = time.range[2]
      )
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

    observe({
      if (is.null(input$wav.file)) {
        return(NULL)
      }
      updateSelectInput(
        session = session,
        inputId = "pulse",
        choices = c("Select a pulse" = "", pulse_label(borders()))
      )
    })

    observe({
      if (is.null(input$wav.file) | input$pulse == "") {
        return(NULL)
      }
      frequency.value <- borders()@data[input$pulse, c("Ymin", "Ymax")] * 1e-3
      frequency.value <- c(
        frequency.value$Ymin - 5,
        frequency.value$Ymax + 5
      )
      time.value <- borders()@data[input$pulse, c("Xmin", "Xmax")] * 1e3
      time.value <- c(
        time.value$Xmin - 5,
        time.value$Xmax + 5
      )
      updateSliderInput(
        session = session,
        inputId = "frequency",
        value = frequency.value
      )
      updateSliderInput(
        session = session,
        inputId = "time",
        value = time.value
      )
    })

    ramp <- reactive({
      midpoint_ramp(
        spectrogram.raster(),
        mid.point = as.numeric(input$amplitude[1]),
        n = 20
      )
    })

    truth <- data.frame(
      File = character(0),
      Spectrogram = character(0),
      Pulse = character(0),
      Species = character(0),
      Type = character(0),
      stringsAsFactors = FALSE
    )

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
        xlim = input$time / 1e3,
        ylim = input$frequency * 1e3,
        xlab = "Time (s)",
        ylab = "Frequency (Hz)"
      )
      lines(borders())
    })
  }
)
