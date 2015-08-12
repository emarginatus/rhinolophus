#' Convert the output of read_wav to a spectrogram
#' @param wav An object as created by \code{\link{read_wav}}
#' @param window.ms The size of the window in microseconds. Default to 1.
#' @importFrom signal specgram
#' @export
wav_2_spectrogram <- function(wav, window.ms = 1){
  window.n <- next_power_2(wav$sample.rate * window.ms / 1000)
  spectrogram <- specgram(
    x = wav$values,
    n = window.n,
    Fs = wav$sample.rate,
    overlap = ceiling(0.9 * window.n)
  )
  spectrogram$S <- 20 * log10(abs(spectrogram$S))
  spectrogram$S <- spectrogram$S - median(spectrogram$S)
  return(spectrogram)
}
