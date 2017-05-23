library(shiny)
library(dplyr)
library(rhinolophus)
library(raster)
shinyServer(function(input, output, session) {
  data <- reactiveValues(
    filename = character(0),
    species = character(0)
  )

  observeEvent(
    input$update_path,
    {
      updateSliderInput(session, "timeinterval", value = 200)
      data$filename <- list.files(
        path = input$path,
        pattern = "\\.wav$",
        ignore.case = TRUE,
        full.names = TRUE
      )[1]
    }
  )

  sonor <- reactive({
    if (length(data$filename) == 0) {
      return(NULL)
    }
    sonogram <- read_wav(data$filename, channel = "left", te.factor = 1) %>%
      wav2spectrogram()
    sonogram$f <- sonogram$f / 1000
    sonogram$t <- sonogram$t * 1000
    updateSliderInput(
      session,
      "starttime",
      value = 0,
      max = input$timeinterval * (max(sonogram$t) %/% input$timeinterval),
      step = input$timeinterval
    )
    amplitude_range <- pretty(range(sonogram$S), 10)
    updateSliderInput(
      session,
      "amplitude",
      value = c(0, max(amplitude_range)),
      min = min(amplitude_range),
      max = max(amplitude_range)
    )
    frequency_range <- pretty(range(sonogram$f), 10)
    updateSliderInput(
      session,
      "frequency",
      value = c(0, pmin(140, max(frequency_range))),
      min = min(frequency_range),
      max = max(frequency_range)
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
    if (is.null(sonor())) {
      return(NULL)
    }
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
      main = data$filename
    )
    abline(h = c(20, 30, 40, 50, 60, 80, 90, 110), lty = 2)
    abline(h = c(18, 21, 27, 35), lty = 3)
  })

  observeEvent(
    input$add_species,
    {
      data$species <- sort(unique(c(data$species, input$new_species)))
      updateCheckboxGroupInput(session, "species", choices = data$species)
    }
  )

  observeEvent(
    input$move_file,
    {
      if (length(input$species) == 0) {
        return(NULL)
      }
      file.rename(
        data$filename,
        sprintf(
          "%s/%s/%s",
          dirname(data$filename),
          paste(input$species, collapse = "_"),
          basename(data$filename)
        )
      )
      updateSliderInput(session, "timeinterval", value = 200)
      data$filename <- list.files(
        path = input$path,
        pattern = "\\.wav$",
        ignore.case = TRUE,
        full.names = TRUE
      )[1]
      updateCheckboxGroupInput(session, "species", choices = data$species, selected = NULL)
    }
  )
})
