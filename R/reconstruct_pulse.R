#' reconstruct a pulse
#' @export
#' @param pulse.fft The fourier representation of the pulse
#' @param local.time Should the pulse be centered (TRUE) or display the original timing (FALSE)
#' @param plot.it Should the reconstructed pulse be plotted?
#' @importFrom raster raster
reconstruct.pulse <- function(pulse.fft, local.time = TRUE, plot.it = TRUE){
  fft.coefs <- matrix(0, nrow = pulse.fft$fft.dim[1], ncol = pulse.fft$fft.dim[2])
  max.dim <- pmin(dim(pulse.fft$fft), pulse.fft$fft.dim)
  fft.coefs[seq_len(max.dim[1]), seq_len(max.dim[2])] <- pulse.fft$fft[seq_len(max.dim[1]), seq_len(max.dim[2])]
  amplitude <- abs(fft(fft.coefs, inverse = TRUE) / prod(pulse.fft$fft.dim))
  extra.time <- ifelse(local.time, 0, pulse.fft$start.time)
  puls.raster <- raster(
    amplitude[rev(seq_len(pulse.fft$fft.dim[1])), ],
    xmn = extra.time,
    xmx = extra.time + pulse.fft$puls.duration,
    ymn = pulse.fft$frequency.range[1],
    ymx = pulse.fft$frequency.range[2]
  )
  if(plot.it){
    plot(puls.raster, xlab = 'Time (ms)', ylab = "Frequency (kHz)", asp = 1)
  }
  return(invisible(puls.raster))
}
