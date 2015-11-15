#' combine the data from several rda files into a object
#' @param path the path in which to look recursively
#' @param keep.spectrogram TRUE keeps the spectrograms, FALSE discards them
#' @importFrom assertthat assert_that is.string is.flag noNA
#' @export
combine_rda <- function(path, keep.spectrogram = FALSE){
  assert_that(is.string(path))
  assert_that(is.flag(keep.spectrogram))
  assert_that(noNA(keep.spectrogram))

  filenames <- list.files(
    path = path,
    pattern = "\\.rda$",
    recursive = TRUE,
    ignore.case = TRUE,
    full.names = TRUE
  )

  pulse.list <- lapply(
    filenames,
    function(filename){
      load(filename)
      if (!keep.spectrogram) {
        pulses.fft@Spectrogram <- list()
      }
      return(pulses.fft)
    }
  )

  metadata <- do.call(
    rbind,
    lapply(
      pulse.list,
      function(x){
        x@Metadata
      }
    )
  )
  pulse.metadata <- do.call(
    rbind,
    lapply(
      pulse.list,
      function(x){
        x@PulseMetadata
      }
    )
  )
  spectrogram.metadata <- do.call(
    rbind,
    lapply(
      pulse.list,
      function(x){
        x@SpectrogramMetadata
      }
    )
  )
  pulse.fourier <- do.call(
    c,
    lapply(
      pulse.list,
      function(x){
        x@PulseFourier
      }
    )
  )
  if (keep.spectrogram) {
    spectrogram <- do.call(
      c,
      lapply(
        pulse.list,
        function(x){
          x@Spectrogram
        }
      )
    )
  } else {
    spectrogram <- list()
  }
  new(
    "batPulse",
    Metadata = metadata,
    SpectrogramMetadata = spectrogram.metadata,
    PulseMetadata = pulse.metadata,
    PulseFourier = pulse.fourier,
    Spectrogram = spectrogram
  )
}
