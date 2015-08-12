context("wav_2_spectrogram")
expect_error(wav_2_spectrogram("a"), "wav is not a list")
expect_error(
  wav_2_spectrogram(list(values = 1)),
  "wav does not have name sample.rate"
)
expect_error(
  wav_2_spectrogram(list(sample.rate = 1)),
  "wav does not have name values"
)
expect_error(
  wav_2_spectrogram(list(sample.rate = "a", values = 1)),
  "wav\\$sample.rate is not a number \\(a length one numeric vector\\)"
)
expect_error(
  wav_2_spectrogram(list(sample.rate = 1, values = "a")),
  "wav\\$values is not a numeric or integer vector"
)
expect_error(
  wav_2_spectrogram(list(sample.rate = 1, values = 1), window.ms = "a"),
  "window.ms is not a number \\(a length one numeric vector\\)"
)
expect_error(
  wav_2_spectrogram(list(sample.rate = 1, values = 1), window.ms = 0),
  "window.ms not greater than 0"
)

dir <- tempdir()
file.copy(
  from = system.file("demo_wav/leislers.wav", package = "rhinolophus"),
  to = dir
)
leislers.file <- paste(dir, "leislers.wav", sep = "/")
spectrogram <- wav_2_spectrogram(wav = read_wav(leislers.file))
expect_is(spectrogram, "specgram")
