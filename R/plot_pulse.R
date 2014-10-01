#' reconstruct a pulse
#' @export
#' @param pulse.fft The fourier representation of the pulse
#' @param local.time Should the pulse be centered (TRUE) or display the original timing (FALSE)
#' @param plot.it Should the reconstructed pulse be plotted?
#' @importFrom raster raster
plot.pulse <- function(pulse.fft, local.time = TRUE, plot.it = TRUE){
  fft.coefs <- matrix(0, nrow = pulse.fft$fft.dim[1], ncol = pulse.fft$fft.dim[2])
  fft.coefs[seq_len(nrow(pulse.fft$fft)), seq_len(ncol(pulse.fft$fft))] <- pulse.fft$fft
  amplitude <- abs(fft(fft.coefs, inverse = TRUE) / prod(pulse.fft$fft.dim))
  extra.time <- ifelse(local.time, 0, pulse.fft$start.time) / 1000
  puls.raster <- raster(
    amplitude[rev(seq_len(pulse.fft$fft.dim[1])), ],
    xmn = extra.time,
    xmx = extra.time + pulse.fft$puls.duration / 1000,
    ymn = pulse.fft$frequency.range[1],
    ymx = pulse.fft$frequency.range[2]
  )
  if(plot.it){
    plot(puls.raster, xlab = 'Time (ms)', ylab = "Frequency (kHz)", asp = 1e-3)
  }
  return(invisible(puls.raster))
}
