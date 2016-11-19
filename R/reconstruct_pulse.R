#' reconstruct a pulse
#' @export
#' @param pulse.fft The fourier representation of the pulse
#' @param local.time Should the pulse be centered (TRUE) or display the original timing (FALSE)
#' @param plot.it Should the reconstructed pulse be plotted?
#' @param asp The aspect ratio
#' @importFrom raster raster
#' @importFrom graphics plot
#' @importFrom stats fft
reconstruct.pulse <- function(pulses, fingerprint){
  assert_that(inherits(pulses, "batPulse"))
  assert_that(is.string(fingerprint))
  assert_that(has_name(pulses@PulseFourier, fingerprint))

  box <- pulses %>%
    calculate_box() %>%
    filter_(~Fingerprint == fingerprint)
  fft_coef <- matrix(0, ncol = box$S, nrow = box$S)
  new_coef <- pulses@PulseFourier[[fingerprint]]
  fft_coef[seq_len(nrow(new_coef)), seq_len(ncol(new_coef))] <- new_coef
  amplitude <- fft_coef %>%
    fft(inverse = TRUE) %>%
    '/'(
      dim(pulses@PulseFourier[[fingerprint]]) %>%
        prod()
    ) %>%
    abs()
  image(amplitude)

  extra.time <- ifelse(local.time, 0, pulse.fft$parameters["StartTime"])
  puls.raster <- raster(
    amplitude[rev(seq_len(pulse.fft$fft.dim[1])), ],
    xmn = extra.time,
    xmx = extra.time + pulse.fft$parameters["PulsDuration"],
    ymn = pulse.fft$parameters["FrequencyMin"],
    ymx = pulse.fft$parameters["FrequencyMax"]
  )
  if(plot.it){
    plot(puls.raster, xlab = 'Time (ms)', ylab = "Frequency (kHz)", asp = asp)
  }
  return(invisible(puls.raster))
}
