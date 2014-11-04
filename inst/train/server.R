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

  output$i <- renderUI({
    current.next.nosave <- input$next.nosave
    current.next.save <- input$next.save
    old.next.save <- get("next.save", envir = train.env)
    prediction <- get("predictions", envir = train.env)
    if(current.next.save > old.next.save){
      to.do <- get("to.do", envir = train.env)
      truth <- get("truth", envir = train.env)
      truth$type[to.do[input$i]] <- input$types
      truth$species[to.do[input$i]] <- input$species
      predictions$predictions <- predictions$predictions[-input$i, ]
      assign("predictions", predictions, envir = train.env)
      assign("truth", truth, envir = train.env)
      assign("to.do", to.do[-input$i], envir = train.env)
      assign("next.save", input$next.save, envir = train.env)
    }
    numericInput(
     "i",
      label = "Pulse",
      value = sample.pulse(predictions)
    )
  })

  plotdata <- reactive({
    if(is.null(input$i)){
      NULL
    } else {
      to.do <- get("to.do", envir = train.env)
      pattern <- get("pattern", envir = train.env)
      prepareplot(pattern[to.do[input$i], ])
    }
  })


  output$amplitude <- renderUI({
    if(is.null(input$i)){
      sliderInput(
        "amplitude",
        label = "amplitude (rel dB)",
        min = 0,
        max = 100,
        value = c(0, 100),
        round = -1
      )
    } else {
      sliderInput(
        "amplitude",
        label = "amplitude (rel dB)",
        min = pmin(0, floor(plotdata()$amplitude[1])),
        max = ceiling(plotdata()$amplitude[4]),
        value = plotdata()$amplitude[2:3],
        round = -1
      )
    }
  })

  output$time <- renderUI({
    if(is.null(input$i)){
      min.time <- 0
      max.time <- 1e3
      range.time <- c(min.time, max.time)
    } else {
      min.time <- plotdata()$time[1] * 1e3
      max.time <- plotdata()$time[4] * 1e3
      range.time <- plotdata()$time[2:3] * 1e3 + c(-50, 50)
    }
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
      max = ceiling(max.time),
      value = range.time,
      round = 1,
      locale = "fr"
    )
  })

  output$frequency <- renderUI({
    if(is.null(input$i)){
      min.freq <- 0
      max.freq <- 220
      range.freq <- c(min.freq, max.freq)
    } else {
      min.freq <- plotdata()$frequency[1] * 1e-3
      max.freq <- plotdata()$frequency[4] * 1e-3
      range.freq <- plotdata()$frequency[2:3] * 1e-3 + c(-5, 5)
    }
    range.freq <- pmin(
      pmax(range.freq, min.freq),
      max.freq
    )
    sliderInput(
      "frequency",
      label = "frequency (kHz)",
      min = min.freq,
      max = ceiling(max.freq),
      value = range.freq,
      round = -1,
      locale = "fr"
    )
  })

  output$types <- renderUI({
    prediction <- get("predictions", envir = train.env)
    if(is.null(input$i)){
      radioButtons(
        "types", label = "type of sound",
        choices = predicted.types(predictions$predictions[1, ])
      )
    } else {
      radioButtons(
        "types", label = "type of sound",
        choices = predicted.types(predictions$predictions[input$i, ])
      )
    }
  })

  output$species <- renderUI({
    prediction <- get("predictions", envir = train.env)
    if(is.null(input$i)){
      radioButtons(
        "species", label = "species",
        choices = predicted.species(predictions$predictions[1, ])
      )
    } else {
      radioButtons(
        "species", label = "species",
        choices = predicted.species(predictions$predictions[input$i, ])
      )
    }
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
