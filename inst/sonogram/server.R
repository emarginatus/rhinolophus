library(shiny)
library(dplyr)
library(rhinolophus)
library(raster)
shinyServer(function(input, output) {
  sonor <- reactive({
    sonogram <- list.files(
      path = input$path,
      pattern = "\\.wav$",
      ignore.case = TRUE,
      full.names = TRUE
    )[1] %>%
      read_wav(channel = "left", te.factor = 1) %>%
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
      clamp(sonor(), lower = 10),
      asp = 10,
      col = topo.colors(100),
      xlim = c(0, 500),
      ylim = c(40, 60),
      xlab = "tims (ms)",
      ylab = "frequency (kKz)"
    )
  })
})
