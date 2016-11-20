#' reconstruct a pulse
#' @export
#' @param pulses The fourier representation of the pulse
#' @param fingerprint
#' @importFrom assertthat assert_that is.string has_name is.flag noNA
#' @importFrom dplyr %>% filter_
reconstruct.pulse <- function(pulses, fingerprint, plot.it = TRUE){
  assert_that(inherits(pulses, "batPulse"))
  assert_that(is.string(fingerprint))
  assert_that(has_name(pulses@PulseMatrix, fingerprint))
  assert_that(is.flag(plot.it))
  assert_that(noNA(plot.it))

  box <- pulses %>%
    calculate_box() %>%
    filter_(~Fingerprint == fingerprint)
  puls.raster <- pulses@PulseMatrix[[fingerprint]] %>%
    "*"(box$DeltaAmplitude) %>%
    "+"(box$MaxAmplitude - box$DeltaAmplitude) %>%
    as.matrix() %>%
    raster(xmn = box$BXmin, xmx = box$BXmax, ymn = box$Ymin, ymx = box$BYmax)
  if (plot.it) {
    plot(puls.raster, xlab = 'Time (ms)', ylab = "Frequency (kHz)")
  }
  return(invisible(puls.raster))
}
