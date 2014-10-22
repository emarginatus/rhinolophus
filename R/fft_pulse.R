#' Calculate the fast fourier transformation for each pulse
#' @export
#' @importFrom sp bbox
#' @param pulses A list of pulses. E.g. the output of \code{\link{contour2pulse}}
#' @param spectrogram The spectrogram. E.g. the output of \code{\link{wav.2.spectrogram}}
#' @param n.fourier The number of required Fourier components in each direction.
fft.pulse <- function(pulses, spectrogram, n.fourier = 30){
  lapply(
    pulses,
    function(pulse){
      bounding.box <- bbox(pulse)
      select.t <- which(bounding.box[1, 1] <= spectrogram$t * 1000 & spectrogram$t * 1000 <= bounding.box[1, 2])
      select.f <- which(bounding.box[2, 1] <= spectrogram$f / 1000 & spectrogram$f / 1000 <= bounding.box[2, 2])
      selection <- spectrogram$S[select.f, select.t, drop = FALSE]
      pulse.fft <- fft(selection)
      full <- matrix(0, ncol = n.fourier, nrow = n.fourier)
      select.dim <- pmin(n.fourier, dim(pulse.fft))
      full[seq_len(select.dim[1]), seq_len(select.dim[2])] <- pulse.fft[seq_len(select.dim[1]), seq_len(select.dim[2])]
      list(
        start.time = bounding.box[1, 1],
        puls.duration = as.vector(diff(bounding.box[1, ])),
        time.delta = diff(spectrogram$t[1:2]) * 1000,
        frequency.range = bounding.box[2, ],
        frequency.delta = diff(spectrogram$f[1:2]) / 1000,
        fft.dim = dim(selection),
        fft = full
      )
    }
  )
}
