predicted.types <- function(x){
  used <- with(
    unique(truth[-to.do, c("type", "species")]),
    data.frame(
      type = type,
      species = species,
      combination = sprintf("%s:%s", type, species)
    )
  )
  used <- used[with(used, order(type, species)), ]
  ranking <- names(x)[order(x, decreasing = TRUE)]
  replace.other <- unique(used$type[!used$combination %in% ranking])
  replace.other <- levels(replace.other)[replace.other]
  known <- gsub(":.*$", "", ranking)
  position.other <- which(ranking == "other:")
  if(position.other == 1){
    unique(c(replace.other, tail(known, -1)))
  } else if(position.other == length(known)){
    unique(c(head(known, -1), replace.other))
  } else {
    unique(c(
      head(known, position.other - 1),
      replace.other,
      tail(known, - length(known) + position.other)
    ))
  }
}


predicted.species <- function(x){
  used <- with(
    unique(truth[-to.do, c("type", "species")]),
    data.frame(
      type = type,
      species = species,
      combination = sprintf("%s:%s", type, species)
    )
  )
  used <- used[with(used, order(species, type)), ]
  ranking <- names(x)[order(x, decreasing = TRUE)]
  replace.other <- unique(used$species[!used$combination %in% ranking])
  replace.other <- levels(replace.other)[replace.other]
  known <- gsub("^.*:", "", ranking)
  position.other <- which(ranking == "other:")
  if(position.other == 1){
    unique(c(replace.other, tail(known, -1)))
  } else if(position.other == length(known)){
    unique(c(head(known, -1), replace.other))
  } else {
    unique(c(
      head(known, position.other - 1),
      replace.other,
      tail(known, - length(known) + position.other)
    ))
  }
}


shinyServer(function(input, output, session) {

  plotdata <- reactive({
    prepareplot(pattern[input$i, ])
  })

  observe({
    junk <- input$next.nosave
    updateNumericInput(session, "i", value = sample.pulse(predictions))
  })

  output$amplitude <- renderUI({
    sliderInput(
      "amplitude",
      label = "amplitude (rel dB)",
      min = plotdata()$amplitude[1],
      max = plotdata()$amplitude[4],
      value = plotdata()$amplitude[2:3],
      round = -1
    )
  })

  output$time <- renderUI({
    min.time <- plotdata()$time[1] * 1e3
    max.time <- plotdata()$time[4] * 1e3
    range.time <- plotdata()$time[2:3] * 1e3 + c(-50, 50)
    range.time <- pmin(
      max.time,
      pmax(
        min.time,
        range.time
      )
    )
    sliderInput(
      "time",
      label = "time (ms)",
      min = min.time,
      max = max.time,
      value = range.time,
      round = 1,
      locale = "fr"
    )
  })

  output$frequency <- renderUI({
    min.freq <- plotdata()$frequency[1] * 1e-3
    max.freq <- plotdata()$frequency[4] * 1e-3
    range.freq <- plotdata()$frequency[2:3] * 1e-3 + c(-5, 5)
    range.freq <- pmin(
      pmax(range.freq, min.freq),
      max.freq
    )
    sliderInput(
      "frequency",
      label = "frequency (kHz)",
      min = min.freq,
      max = max.freq,
      value = range.freq,
      round = -1,
      locale = "fr"
    )
  })

  output$types <- renderUI({
      radioButtons(
        "types", label = "type of sound",
        choices = predicted.types(predictions$predictions[input$i, ])
      )
  })

  output$species <- renderUI({
      radioButtons(
        "species", label = "species",
        choices = predicted.species(predictions$predictions[input$i, ])
      )
  })

  output$pulsePlot <- renderPlot(
    {
      dataset <- plotdata()
      plotpattern(
        dataset,
        amplitudes = input$amplitude,
        times = input$time * 1e-3,
        frequencies = input$frequency * 1e3
      )
    }, height = 800
  )
})
