library(shiny)
library(rhinolophus)
library(dplyr)

shinyServer(
  function(input, output){
    pattern <- reactive({
      if (is.null(input$pulse.file)) {
        return(
          list(
            Pulse = data.frame(
              Fingerprint = character(0),
              Filename = character(0),
              Spectrogram = character(0)
            )
          )
        )
      }
      batpulse_rda2list(input$pulse.file$datapath)
    })

    border <- reactive({
      if (is.null(input$pulse.file)) {
        return(NULL)
      }
      pulse_border(pattern()$Pulse)
    })

    pulse.id <- reactive({
      if (is.null(input$pulse.file)) {
        return(character(0))
      }
      pattern()$Pulse$Fingerprint[input$i]
    })

    spectrogram.id <- reactive({
      pattern()$Pulse %>%
        filter_(~Fingerprint == pulse.id()) %>%
        select_(~Spectrogram) %>%
        unlist()
    })

    spectrogram <- reactive({
      pattern()$Spectrogram[[spectrogram.id()]]
    })

    spectrogram.raster <- reactive({
        spectrogram_raster(spectrogram())
    })

    output$filename <- renderText(
      pattern()$Pulse %>%
        filter_(~Fingerprint == pulse.id()) %>%
        select_(~Filename) %>%
        unlist()
    )
    output$plot.pulse <- renderPlot({
      if (is.null(input$pulse.file)) {
        return(NULL)
      }

      this.border <- border() %>%
        filter_(~Fingerprint == pulse.id()) %>%
        select_(~X, ~Y)
      this.border$X <- spectrogram()$t[this.border$X]
      this.border$Y <- spectrogram()$f[this.border$Y]

      plot(
        spectrogram.raster(),
        asp = 0.5e-6,
        xlim = range(this.border$X),
        ylim = range(this.border$Y)
      )
      lines(this.border)
    })
  }
)
