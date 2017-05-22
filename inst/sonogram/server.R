library(shiny)
library(dplyr)
library(rhinolophus)
library(raster)
shinyServer(function(input, output) {
  filename <- reactive({
    list.files(
      path = input$path,
      pattern = "\\.wav$",
      ignore.case = TRUE,
      full.names = TRUE
    )[1]
  })

  sonor <- reactive({
    sonogram <- read_wav(filename(), channel = "left", te.factor = 1) %>%
      wav2spectrogram()
    sonogram$f <- sonogram$f / 1000
    sonogram$t <- sonogram$t * 1000
    raster(
      sonogram$S[rev(seq_along(sonogram$f)), ],
      xmn = min(sonogram$t),
      xmx = max(sonogram$t),
      ymn = min(sonogram$f),
      ymx = max(sonogram$f)
    )
  })

  output$sonogram <- renderPlot({
    plot(
      clamp(sonor(), lower = input$amplitude),
      asp = 1,
      col = topo.colors(100),
      xlim = c(0, 500),
      ylim = input$frequency,
      xlab = "time (ms)",
      ylab = "frequency (kHz)",
      main = filename()
    )
  })
})
