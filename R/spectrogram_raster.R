#' Convert a spectrogram to a raster object
#' @export
#' @param spectrogram an object of the class specgram
#' @importFrom assertthat assert_that
#' @importFrom raster raster
spectrogram_raster <- function(spectrogram){
  assert_that(inherits(spectrogram, "specgram"))

  spectrogram.raster <- raster(
    spectrogram$S[rev(seq_len(nrow(spectrogram$S))), ],
    xmn = min(spectrogram$t) * 1e3,
    xmx = max(spectrogram$t) * 1e3,
    ymn = min(spectrogram$f) * 1e-3,
    ymx = max(spectrogram$f) * 1e-3
  )
  names(spectrogram.raster) <- "dB"

  return(spectrogram.raster)
}
