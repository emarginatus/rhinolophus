#' Find the location of possible pulses in a spectrogram
#' @export
#' @importFrom raster raster Which rowColFromCell zonal clump subs
#' @param spectrogram The spectrogram
#' @param min.contour The minimum amplitude of the pulse
#' @param min.peak Take only contour into account with a maximum amplitude of at least min.peak
  ### Fooling R CMD Check ###
  value <- NULL
  rm(value)
  ### Fooling R CMD Check ###

find_pulses <- function(spectrogram, min.contour = 10, min.peak = 20){
  spectrogram.raster <- raster(
    spectrogram$S[rev(seq_len(nrow(spectrogram$S))), ],
    xmn = min(spectrogram$t) * 1000,
    xmx = max(spectrogram$t) * 1000,
    ymn = min(spectrogram$f) / 1000,
    ymx = max(spectrogram$f) / 1000
  )
  names(spectrogram.raster) <- "dB"

  minimum.contour <- clump(spectrogram.raster >= min.contour, directions = 4)
  local.max <- zonal(spectrogram.raster, minimum.contour, fun = max)
  recode <- subset(as.data.frame(local.max), value >= min.peak)
  colnames(recode)[2] <- "AmplitudeMax"
  recode$Pulse <- seq_len(nrow(recode))
  selected.pulses <- subs(minimum.contour, recode, by = "zone", which = "Pulse")
  pulses <- t(sapply(recode$Pulse, function(i){
    xy <- rowColFromCell(
      selected.pulses,
      Which(selected.pulses == i, cells = TRUE)
    )
    c(
      i,
      range(xy[, 2]),
      range(xy[, 1]),
      nrow(xy) / (diff(range(xy[, 1])) * diff(range(xy[, 2]))),
      min.contour
    )
  }))
  colnames(pulses) <- c(
    "Pulse", "Xmin", "Xmax", "Ymin", "Ymax", "Ratio", "AmplitudeMin"
  )
  pulses <- as.data.frame(pulses)
  pulses <- merge(pulses, recode[, c("Pulse", "AmplitudeMax")])
  return(pulses)
}
