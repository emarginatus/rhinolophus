library(shiny)
library(dplyr)
library(rhinolophus)
library(raster)
shinyServer(function(input, output, session) {
  filename <- eventReactive(
    input$update_path,
    {
      updateSliderInput(session, "timeinterval", value = 200)
      updateSliderInput(session, "frequence", value = c(0, 150))
      list.files(
        path = input$path,
        pattern = "\\.wav$",
        ignore.case = TRUE,
        full.names = TRUE
      )[1]
    }
  )

  sonor <- reactive({
    sonogram <- read_wav(filename(), channel = "left", te.factor = 1) %>%
      wav2spectrogram()
    sonogram$f <- sonogram$f / 1000
    sonogram$t <- sonogram$t * 1000
    updateSliderInput(
      session,
      "starttime",
      value = 0,
      max = input$timeinterval * (max(sonogram$t) %/% input$timeinterval)
    )
    amplitude_range <- pretty(range(sonogram$S), 10)
    updateSliderInput(
      session,
      "amplitude",
      value = c(0, max(amplitude_range)),
      min = min(amplitude_range),
      max = max(amplitude_range)
    )
    raster(
      sonogram$S[rev(seq_along(sonogram$f)), ],
      xmn = min(sonogram$t),
      xmx = max(sonogram$t),
      ymn = min(sonogram$f),
      ymx = max(sonogram$f)
    )
  })

  observeEvent(
    input$step_backward,
    updateSliderInput(
      session,
      "starttime",
      value = input$starttime - input$timeinterval
    )
  )

  observeEvent(
    input$step_forward,
    updateSliderInput(
      session,
      "starttime",
      value = input$starttime + input$timeinterval
    )
  )

  output$sonogram <- renderPlot({
    breaks <- pretty(input$amplitude[1]:input$amplitude[2], 20)
    plot(
      clamp(sonor(), lower = input$amplitude[1], upper = input$amplitude[2]),
      asp = 1,
      breaks = breaks,
      col = topo.colors(length(breaks)),
      xlim = input$starttime + c(0, input$timeinterval),
      ylim = input$frequency,
      xlab = "time (ms)",
      ylab = "frequency (kHz)",
      main = filename()
    )
    abline(h = c(20, 30, 40, 50, 60, 80, 90, 110), lty = 2)
    abline(h = c(18, 21, 27, 35), lty = 3)
  })
})
