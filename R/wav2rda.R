#' Find the pulses in a bunch of wav files and save them to rda files
#' @export
#' @param path the path of the wav files
#' @param time.expansion.factor The time-expansion factor used in the wav files. Defaults to 10
#' @param channel Which channel will be read. Defaults to "right"
#' @param window.ms The size of the window for fourier transformation of the sound in milliseconds. The time-expansion factor is taken into account. Defaults to 1.
#' @param min.contour The minimum amplitude to mark the start of a pulse. Expressed in dB above the median amplitude of the wav file.
#' @param min.peak The required maximum amplitude of a pulse. Pulses with a maximum amplitude below min.peak will be ignored.
wav2rda <- function(path, time.expansion.factor = 10, channel = "right", window.ms = 2, min.contour = 10, min.peak = 20, n.fourier = 30){
  filenames <- list.files(
    path = path,
    pattern = "\\.wav$",
    recursive = TRUE,
    ignore.case = TRUE,
    full.names = TRUE
  )
  for(filename in filenames){
    message(filename)
    wav <- read.wav(
      filename = filename,
      time.expansion.factor = time.expansion.factor,
      channel = channel
    )
    spectrogram <- wav.2.spectrogram(
      wav = wav,
      window.ms = window.ms
    )
    rm(wav)
    pulses <- find.pulses(
      spectrogram = spectrogram,
      min.contour = min.contour,
      min.peak = min.peak
    )
    pulses.fft <- fft.pulse(
      pulses = pulses,
      spectrogram = spectrogram,
      n.fourier = n.fourier
    )
    rm(pulses)
    save(pulses.fft, time.expansion.factor, channel, window.ms, min.contour, min.peak, n.fourier, file = gsub("(WAV|wav)$", "rda", filename))
    rm(pulses.fft)
    gc()
  }
}
