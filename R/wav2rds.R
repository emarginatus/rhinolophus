#' Find the pulses in a bunch of wav files and save them to rda files
#' @export
#' @param path The path of the wav files. The function will reach recursive in
#'    the subdirectories of the path.
#' @param action What to do when a rds file exists? \code{"append"} will add only new information. Existing information will be intact. \code{"update"} will keep all information and update existing information. \code{"overwrite"} remove the rds file and creates a new one. Defaults to \code{"append"}.
#' @inheritParams read_wav
#' @inheritParams wav2spectrogram
#' @inheritParams find_pulses
#' @inheritParams fft_pulse
#' @importFrom assertthat assert_that is.string is.flag noNA
#' @importFrom dplyr %>% semi_join filter_ bind_rows
#' @importFrom methods validObject
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
  action = c("append", "update", "overwrite")
){
  assert_that(is.string(path))
  channel <- match.arg(channel)
  action <- match.arg(action)

  filenames <- list.files(
    path = path,
    pattern = "\\.wav$",
    recursive = TRUE,
    ignore.case = TRUE,
    full.names = TRUE
  )

  for (filename in filenames) {
    message(filename)

    rds.file <- filename %>%
      gsub(pattern = "\\.wav$", replacement = ".rds", ignore.case = TRUE)
    needwav <- TRUE
    if (action != "overwrite" && file_test("-f", rds.file)) {
      current <- readRDS(rds.file)
      spec.metadata <- current@SpectrogramMetadata %>%
        semi_join(
          current@Metadata %>%
            filter_(
              ~Filename == filename,
              ~MaxLength == max.length,
              ~Channel == channel,
              ~TimeExpansionFactor == te.factor
            ),
          by = c("File" = "Fingerprint")
        ) %>%
        filter_(~Window == window.ms)
      if (spec.metadata$Fingerprint %in% names(current@Spectrogram)) {
        if (action == "append") {
          available <- current@PulseMetadata %>%
            semi_join(
              spec.metadata,
              by = c("Spectrogram" = "Fingerprint")
            )
          if (any(available$DeltaAmplitude == delta.amplitude)) {
            next
          }
        } else {
          stop("todo: remove existing pulses")
        }
        needwav <- FALSE
      }
    }
    if (needwav) {
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
    } else {
      spectrogram <- new(
        "batSpectrogram",
        Metadata = current@Metadata,
        SpectrogramMetadata = current@SpectrogramMetadata,
        Spectrogram = current@Spectrogram
      )
    }
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
    if (exists("current")) {
      pulses.fft@PulseMetadata <- bind_rows(
        pulses.fft@PulseMetadata %>%
          mutate_(Spectrogram = ~levels(Spectrogram)[Spectrogram]),
        current@PulseMetadata %>%
          mutate_(Spectrogram = ~levels(Spectrogram)[Spectrogram])
      ) %>%
        mutate_(Spectrogram = ~factor(Spectrogram))
      pulses.fft@PulseFourier <- c(
        pulses.fft@PulseFourier,
        current@PulseFourier
      )
      validObject(pulses.fft)
      rm(current)
    }
    saveRDS(
      pulses.fft,
      file = rds.file
    )
    rm(pulses.fft)
    gc()
  }
}
