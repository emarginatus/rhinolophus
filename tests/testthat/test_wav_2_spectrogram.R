context("wav2spectrogram")
expect_error(wav2spectrogram("a"), "wav does not inherit from class batWav")
wav <- read_wav(
  system.file("demo_wav/leislers.wav", package = "rhinolophus")
)
expect_error(
  wav2spectrogram(wav, window.ms = "a"),
  "window.ms is not a number \\(a length one numeric vector\\)"
)
expect_error(
  wav2spectrogram(wav, window.ms = 0),
  "window.ms not greater than 0"
)

expect_is(
  wav2spectrogram(wav),
  "batSpectrogram"
)
empty <- wav2spectrogram(
  wav = read_wav(
    system.file("demo_wav/leislers.wav", package = "rhinolophus"),
    channel = "right"
  )
)
expect_identical(length(empty@Spectrogram), 0L)
expect_identical(nrow(empty@SpectrogramMetadata), 0L)
rm(wav, empty)
