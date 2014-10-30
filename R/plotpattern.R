#' Plot a single pattern
#' @export
#' @param plotdata The output of prepareplot
#' @param amplitudes Limits of amplitudes. Lower amplitudes will be gray, higer amplitudes will be white.
#' @param times Limits of time
#' @param frequencies Limits of frequencies
#' @param reference Frequecies of reference lines to plot
plotpattern <- function(plotdata, amplitudes, times, frequencies, reference = c(30, 40, 45, 50, 55) * 1e3){
  if(is.null(plotdata)){
    return()
  }
  if(missing(amplitudes) | is.null(amplitudes)){
    amplitudes <- plotdata$amplitude[2:3]
  }
  if(missing(times) | length(times) == 0){
    times <- plotdata$time[2:3] + c(-.05, .05)
  }
  if(missing(frequencies) | length(frequencies) == 0){
    frequencies <- plotdata$frequency[2:3] + c(-1e4, 1e4)
  }
  amplitudes <- pmax(1e-20, amplitudes)
  breaks <- c(plotdata$amplitude[1] + 1e-20, seq(amplitudes[1], amplitudes[2], length = 11))
  colours <- c("#888888", rainbow(10, end = amplitudes[2] / 100))
  breaks <- 20 * log10(breaks)
  plot(
    plotdata$spectrogram,
    xlim = times,
    ylim = frequencies,
    breaks = breaks,
    col = colours
  )
  segments(
    x0 = plotdata$time[2:3], y0 = plotdata$frequency[2],
    x1 = plotdata$time[2:3], y1 = plotdata$frequency[3],
    col = "black", lwd = 3
  )
  segments(
    x0 = plotdata$time[2], y0 = plotdata$frequency[2:3],
    x1 = plotdata$time[3], y1 = plotdata$frequency[2:3],
    col = "black", lwd = 3
  )
  abline(h = reference, lty = 2, col = "white")
}
