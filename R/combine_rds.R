#' combine the data from several rda files into a object
#' @param path the path in which to look recursively
#' @param keep.spectrogram TRUE keeps the spectrograms, FALSE discards them
#' @importFrom assertthat assert_that is.string is.flag noNA
#' @importFrom methods new
#' @export
combine_rds <- function(path, keep.spectrogram = FALSE){
  assert_that(is.string(path))
  assert_that(is.flag(keep.spectrogram))
  assert_that(noNA(keep.spectrogram))

  filenames <- list.files(
    path = path,
    pattern = "\\.rds$",
    recursive = TRUE,
    ignore.case = TRUE,
    full.names = TRUE
  )

  pulse.list <- lapply(
    filenames,
    function(filename){
      pulses.fft <- readRDS(filename)
      if (!keep.spectrogram) {
        pulses.fft@Spectrogram <- list()
      }
      return(pulses.fft)
    }
  )

  metadata <- lapply(
    pulse.list,
    function(x){
      x@Metadata
    }
  ) %>%
    bind_rows()


  pulse.metadata <- lapply(
    pulse.list,
    function(x){
      y <- x@PulseMetadata
      y$Spectrogram <- levels(y$Spectrogram)[y$Spectrogram]
      return(y)
    }
  ) %>%
    bind_rows() %>%
    mutate_(Spectrogram = ~factor(Spectrogram))

  spectrogram.metadata <- lapply(
    pulse.list,
    function(x){
      y <- x@SpectrogramMetadata
      y$File <- levels(y$File)[1]
      return(y)
    }
  ) %>%
    bind_rows() %>%
    mutate_(File = ~factor(File))

  pulse.fourier <- lapply(
    pulse.list,
    function(x){
      x@PulseFourier
    }
  ) %>%
    do.call(what = c)

  if (keep.spectrogram) {
    spectrogram <- lapply(
      pulse.list,
      function(x){
        x@Spectrogram
      }
    ) %>%
      do.call(what = c)
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
