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
      if (file_test("-f", paste0(input$path, "/_truth.rds"))) {
        truth <- readRDS(paste0(input$path, "/_truth.rds"))
      } else {
        n.pulse <- input$path %>%
          list.files(
            pattern = "\\.rds$",
            full.names = TRUE,
            recursive = TRUE,
            ignore.case = TRUE
          ) %>%
          sapply(
            function(this.filename){
              readRDS(this.filename) %>%
                "@"("PulseMetadata") %>%
                nrow()
            }
          )
        truth <- list(
          files = data.frame(
            filename = names(n.pulse),
            pulses = unname(n.pulse),
            stringsAsFactors = FALSE
          )
        )
        saveRDS(truth, file = paste0(input$path, "/_truth.rds"))
      }
      sample(truth[["files"]]$filename, 1, prob = truth[["files"]]$pulses)
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
      asp = 0.75,
      main = pulses()@Metadata$Filename,
      xlim = c(this.pulse()$BXmin, this.pulse()$BXmax),
      ylim = c(this.pulse()$BYmin, this.pulse()$BYmax),
      xlab = "Time (ms)",
      ylab = "Frequency (kHz)"
  )
    lines(borders())
    points(this.pulse()$Xpeak, this.pulse()$Ypeak, col = "magenta")
  })
})
