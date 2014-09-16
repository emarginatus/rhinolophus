#' Find the location of possible pulses in a spectrogram
#' @export
#' @importFrom zoo rollapply
#' @param spectrogram The spectrogram
#' @param min.kHz Ignore spectogram data below this frequency
#' @param local.peak.ms The time (in milliseconds) over which the current amplitude must be equal to the local maximum or minimum
find.pulses <- function(spectrogram, min.kHz = 10, local.peak.ms = 10){
  frequency <- spectrogram$f / 1000
  max.amplitude <- apply(spectrogram$S[frequency > min.kHz, ], 2, max)
  delta.t <- diff(spectrogram$t[1:2])
  local.width <- ceiling(local.peak.ms * 1e-3 / delta.t)
  local.max.amplitude <- rollapply(max.amplitude, width = local.width, FUN = max, fill = NA)
  local.min.amplitude <- rollapply(max.amplitude, width = local.width, FUN = min, fill = NA)
  which.high <- which(max.amplitude == local.max.amplitude)
  which.low <- which(max.amplitude == local.min.amplitude)
  which.high <- which.high[which.high > min(which.low)]
  which.high <- which.high[which.high < max(which.low)]
  sapply(which.high, function(i){
    c(
      max(which.low[which.low < i]),
      min(which.low[i < which.low])
    )
  })
}
