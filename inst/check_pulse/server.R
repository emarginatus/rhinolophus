library(shiny)
library(dplyr)
library(raster)
library(rhinolophus)

shinyServer(function(input, output, session) {
  v <- reactiveValues(
    truth = NULL,
    current = NULL,
    pulses = NULL,
    borders = NULL,
    boxes = NULL
  )

  observeEvent(
    input$new.path, {
      if (is.null(input$path)) {
        return(NULL)
      }
      if (file_test("-f", paste0(input$path, "/_truth.rds"))) {
        v$truth <- readRDS(paste0(input$path, "/_truth.rds"))
        updateRadioButtons(
          session = session,
          inputId = "species",
          choices = sort(unique(c("noise", na.omit(v$truth$Species))))
        )
        updateRadioButtons(
          session = session,
          inputId = "type",
          choices = sort(unique(
            c("call", na.omit(v$truth$Type))
          ))
        )
      } else {
        v$truth <- input$path %>%
          list.files(
            pattern = "\\.rds$",
            full.names = TRUE,
            recursive = TRUE,
            ignore.case = TRUE
          ) %>%
          lapply(
            function(this.filename){
              rds <- readRDS(this.filename)
              rds@PulseMetadata %>%
                select_(~Spectrogram, ~Fingerprint) %>%
                inner_join(
                  rds@SpectrogramMetadata %>%
                    mutate_(Fingerprint = ~factor(Fingerprint)),
                  by = c("Spectrogram" = "Fingerprint")
                ) %>%
                inner_join(
                  rds@Metadata %>%
                    mutate_(Fingerprint = ~factor(Fingerprint)),
                  by = c("File" = "Fingerprint")
                ) %>%
                transmute_(
                  File = ~gsub("\\.wav$", "\\.rds", Filename, ignore.case = TRUE),
                  ~Fingerprint,
                  Species = NA_character_,
                  Type = NA_character_,
                  SecondOption = NA
                )
            }
          ) %>%
          bind_rows()
        saveRDS(v$truth, file = paste0(input$path, "/_truth.rds"))
      }
      v$filename <- v$truth %>%
        filter_(~is.na(Species)) %>%
        sample_n(1) %>%
        '[['("File")
      v$pulses <- readRDS(v$filename)
      v$spectrogram <- spectrogram_raster(v$pulses@Spectrogram[[1]])
      v$borders <- pulse_border(v$pulses)
      v$boxes <- calculate_box(v$pulses)
      v$this.pulse <- v$boxes %>%
        inner_join(
          v$truth,
          by = "Fingerprint"
        ) %>%
        filter_(~is.na(Species)) %>%
        arrange_(~Ypeak, ~Xpeak) %>%
        slice_(1)
    }
  )

  ramp <- reactive({
    midpoint_ramp(
      v$spectrogram,
      mid.point = v$this.pulse$MaxAmplitude - v$this.pulse$DeltaAmplitude,
      n = 20
    )
  })

  output$plot <- renderPlot({
    if (is.null(v$pulses)) {
      return(character(0))
    }
    plot(
      v$spectrogram,
      asp = 0.75,
      main = v$pulses@Metadata$Filename,
      breaks = ramp()$Breaks,
      col = ramp()$Colour,
      xlim = c(v$this.pulse$BXmin, v$this.pulse$BXmax),
      ylim = c(v$this.pulse$BYmin, v$this.pulse$BYmax),
      xlab = "Time (ms)",
      ylab = "Frequency (kHz)"
    )
    lines(v$borders)
    lines(
      v$borders[v$borders$Fingerprint == v$this.pulse$Fingerprint, ],
      col = "magenta",
      lwd = 2
    )
    points(
      v$this.pulse$Xpeak,
      v$this.pulse$Ypeak,
      col = "magenta",
      cex = 3,
      pch = 13
    )
    abline(h = c(18, 20, 21, 27, 35, 40, 50), col = "blue", lty = 2)
  })

  observeEvent(
    input$new.truth,
    {
      if (is.null(input$path)) {
        return(NULL)
      }

      selection <- v$truth$Fingerprint == v$this.pulse$Fingerprint
      v$truth$Species[selection] <- ifelse(
        input$new.species == "",
        input$species,
        input$new.species
      )
      v$truth$Type[selection] <- ifelse(
        input$new.type == "",
        input$type,
        input$new.type
      )
      if (input$new.species != "") {
        updateRadioButtons(
          session = session,
          inputId = "species",
          choices = sort(unique(c("noise", na.omit(v$truth$Species))))
        )
        updateTextInput(
          session = session,
          inputId = "new.species",
          value = ""
        )
      }
      if (input$new.type != "") {
        updateRadioButtons(
          session = session,
          inputId = "type",
          choices = unique(
            sort(c("call", na.omit(v$truth$Type)))
          )
        )
        updateTextInput(
          session = session,
          inputId = "new.type",
          value = ""
        )
      }
      v$this.pulse <- v$boxes %>%
        inner_join(
          v$truth,
          by = "Fingerprint"
        ) %>%
        filter_(~is.na(Species)) %>%
        arrange_(~Ypeak, ~Xpeak) %>%
        slice_(1)
      if (nrow(v$this.pulse) == 0) {
        saveRDS(v$truth, file = paste0(input$path, "/_truth.rds"))
        v$filename <- v$truth %>%
          filter_(~is.na(Species)) %>%
          sample_n(1) %>%
          '[['("File")
        v$pulses <- readRDS(v$filename)
        v$spectrogram <- spectrogram_raster(v$pulses@Spectrogram[[1]])
        v$borders <- pulse_border(v$pulses)
        v$boxes <- calculate_box(v$pulses)
        v$this.pulse <- v$boxes %>%
          inner_join(
            v$truth,
            by = "Fingerprint"
          ) %>%
          filter_(~is.na(Species)) %>%
          arrange_(~Ypeak, ~Xpeak) %>%
          slice_(1)
      }
    }
  )

})
