#' Convert a spectrogram to a raster object
#' @export
#' @param spectrogram an object of the class specgram
#' @importFrom assertthat assert_that
#' @importFrom raster raster
spectrogram_raster <- function(spectrogram){
  assert_that(inherits(spectrogram, "specgram"))

  spectrogram.raster <- raster(
    spec$S[rev(seq_len(nrow(spec$S))), ],
    xmn = min(spec$t),
    xmx = max(spec$t),
    ymn = min(spec$f),
    ymx = max(spec$f)
  )
  names(spectrogram.raster) <- "dB"

  return(spectrogram.raster)
}
