#' Convert the output of read_wav to a spectrogram
#' @param wav An object as created by \code{\link{read_wav}}
#' @param window.ms The size of the window in microseconds. Default to 1.
#' @importFrom assertthat assert_that has_name is.number
#' @importFrom dplyr %>% filter_ select_
#' @importFrom signal specgram
#' @importFrom digest sha1
#' @export
#' @examples
#'  wav2spectrogram(
#'    wav = read_wav(
#'      system.file("demo_wav/leislers.wav", package = "rhinolophus")
#'    )
#'  )
wav2spectrogram <- function(wav, window.ms = 1){
  assert_that(inherits(wav, "batWav"))
  validObject(wav)
  assert_that(is.number(window.ms))
  assert_that(window.ms > 0)

  spectrogram <- lapply(
    names(wav@Wav),
    function(fingerprint){
      if (length(wav@Wav[[fingerprint]]) == 0) {
        return(NULL)
      }
      sampling.rate <- wav@Metadata %>%
        filter_(~Fingerprint == fingerprint) %>%
        select_(~SamplingRate) %>%
        unlist()
      window.n <- next_power_2(sampling.rate * window.ms / 1000)
      spectrogram <- specgram(
        x = wav@Wav[[fingerprint]],
        n = window.n,
        Fs = sampling.rate,
        overlap = ceiling(0.9 * window.n)
      )
      spectrogram$S <- 20 * log10(abs(spectrogram$S))
      spectrogram$S <- spectrogram$S - median(spectrogram$S)
      return(spectrogram)
    }
  )
  names(spectrogram) <- sapply(
    names(wav@Wav),
    function(x){
      sha1(list(x, window.ms))
    }
  )
  metadata = data.frame(
    Fingerprint = names(spectrogram),
    File = names(wav@Wav),
    Window = window.ms,
    stringsAsFactors = FALSE
  )
  keep <- !sapply(spectrogram, is.null)
  metadata <- metadata[keep, ]
  spectrogram <- spectrogram[keep]
  new(
    "batSpectrogram",
    Metadata = wav@Metadata,
    SpectrogramMetadata = metadata,
    Spectrogram = spectrogram
  )
}
