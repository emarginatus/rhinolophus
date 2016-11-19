#' Extract the parameters of all pulses as a sparse matrix
#' @param pulses a batPulse object
#' @export
#' @importFrom assertthat assert_that
pulse_matrix <- function(pulses){
  assert_that(inherits(pulses, what = "batPulse"))

  if (length(pulses@Spectrogram) != 1) {
    stop("pulse_parameter() handles only one spectrogram at a time.")
  }

  if (nrow(pulses@PulseMetadata) == 0) {
    return(pulses)
  }

  spectrogram.raster <- spectrogram_raster(
    pulses@Spectrogram[[1]]
  )
  matrices <- pulses %>%
    calculate_box() %>%
    rowwise() %>%
    do_(
      Fingerprint = ~.$Fingerprint,
      Matrix = ~interpolate_pulse(
        spectrogram.raster = spectrogram.raster,
        this.box = .
      )
    )
  names(matrices$Matrix) <- matrices$Fingerprint
  pulses@PulseMatrix <- matrices$Matrix
  return(pulses)
}

#' Convert a single pulse to a sparse matrix
#' @param spectrogram.raster a spectrogram raster
#' @param this.box the bounding box of the pulse
#' @importFrom dplyr %>%
#' @importFrom raster extent crop raster resample clump extract Which mask ncol nrow
#' @importMethodsFrom raster as.matrix
#' @importFrom Matrix Matrix
#' @export
interpolate_pulse <- function(spectrogram.raster, this.box){
  resolution <- c(0.5, 0.5)
  dB.range <- this.box$MaxAmplitude - this.box$DeltaAmplitude

  spectrogram <- this.box[c("Xmin", "Xmax", "Ymin", "Ymax")] %>%
    unlist() %>%
    extent() %>%
    crop(x = spectrogram.raster)
  detail.raster <- raster(
    xmn = this.box$BXmin,
    xmx = this.box$BXmax,
    ymn = this.box$BYmin,
    ymx = this.box$BYmax,
    resolution = resolution
  ) %>%
    resample(x = spectrogram)
  candidate.zone <- clump(
    detail.raster >= dB.range,
    directions = 4
  )
  relevant <- this.box[c("Xpeak", "Ypeak")] %>%
    unlist() %>%
    matrix(nrow = 1) %>%
    extract(x = candidate.zone) %>%
    '!='(candidate.zone) %>%
    Which(cells = TRUE)
  candidate.zone[relevant] <- NA
  mask(
    detail.raster,
    candidate.zone,
    updatevalue = dB.range,
    updateNA = TRUE
  ) %>%
    '-'(dB.range) %>%
    '/'(this.box$DeltaAmplitude) %>%
    Matrix(nrow = nrow(detail.raster), ncol = ncol(detail.raster), byrow = TRUE)
}
