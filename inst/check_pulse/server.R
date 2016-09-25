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
      if (file_test("-f", paste0(input$new.path, "/_truth.rds"))) {
        truth <- readRDS(paste0(input$new.path, "/_truth.rds"))
      }
      to.do <- list.files(
        input$path,
        pattern = "\\.rds$",
        full.names = TRUE,
        recursive = TRUE,
        ignore.case = TRUE
      )
      to.do[!grepl("_truth\\.rds$", to.do)] %>%
        sample(1)
  })

  pulses <- reactive({
    if (filename() == "") {
      return(NULL)
    }
    readRDS(file = filename())
  })

  borders <- reactive({
    pulse_border(pulses())
  })

  boxes <- reactive({
    calculate_box(pulses())
  })

  this.pulse <- reactive({boxes()[1, ]})

  spectrogram <- reactive({
    if (is.null(pulses())) {
      return(NULL)
    }
    spectrogram_raster(pulses()@Spectrogram[[1]])
  })

  output$plot <- renderPlot({
    if (is.null(pulses())) {
      return(character(0))
    }
    plot(
      spectrogram(),
      asp = 0.5,
      main = pulses()@Metadata$Filename,
      xlim = c(this.pulse()$BXmin, this.pulse()$BXmax),
      ylim = c(this.pulse()$BYmin, this.pulse()$BYmax),
      xlab = "Time (ms)",
      ylab = "Frequency (kHz)"
  )
    lines(borders())
  })
})
