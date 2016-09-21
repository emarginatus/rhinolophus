#' Find the pulses in a bunch of wav files and save them to rda files
#' @export
#' @param path The path of the wav files. The function will reach recursive in
#'    the subdirectories of the path.
#' @param overwrite Overwrite existing rda files. Defaults to FALSE
#' @inheritParams read_wav
#' @inheritParams wav2spectrogram
#' @inheritParams find_pulses
#' @inheritParams fft_pulse
#' @importFrom assertthat assert_that is.string is.flag noNA
wav2rds <- function(
  path,
  te.factor = 10,
  channel = c("right", "left"),
  max.length = 1.8,
  window.ms = 2,
  min.peak = 40,
  min.amplitude = 10,
  delta.amplitude = 30,
  n.fourier = 30,
  overwrite = FALSE
){
  assert_that(is.string(path))
  channel <- match.arg(channel)
  assert_that(is.flag(overwrite))
  assert_that(noNA(overwrite))

  filenames <- list.files(
    path = path,
    pattern = "\\.wav$",
    recursive = TRUE,
    ignore.case = TRUE,
    full.names = TRUE
  )
  if (!overwrite) {
    rda_filenames <- list.files(
      path = path,
      pattern = "\\.rds$",
      recursive = TRUE,
      ignore.case = TRUE,
      full.names = TRUE
    ) %>%
      gsub(pattern = "\\.rds", replacement = "", ignore.case = TRUE)
    filenames <- filenames[
      !gsub("\\.wav$", "", filenames, ignore.case = TRUE) %in% rda_filenames
    ]
  }

  for (filename in filenames) {
    message(filename)
    wav <- read_wav(
      filename = filename,
      te.factor = te.factor,
      channel = channel,
      max.length = max.length
    )
    if (length(wav@Wav[[1]]) == 0) {
      warning(filename, " contains no information in the ", channel, " channel")
      next
    }
    spectrogram <- wav2spectrogram(
      wav = wav,
      window.ms = window.ms
    )
    rm(wav)
    pulses <- find_pulses(
      spectrogram = spectrogram,
      min.peak = min.peak,
      min.amplitude = min.amplitude,
      delta.amplitude = delta.amplitude
    )
    rm(spectrogram)
    pulses.fft <- fft_pulse(
      pulses = pulses,
      n.fourier = n.fourier
    )
    rm(pulses)
    saveRDS(
      pulses.fft,
      file = gsub("\\.wav$", ".rds", filename, ignore.case = TRUE)
    )
    rm(pulses.fft)
    gc()
  }
}
