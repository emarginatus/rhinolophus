context("find_pulses")
wav <- read_wav(
  system.file("demo_wav/leislers.wav", package = "rhinolophus")
)
spectrogram <- wav_2_spectrogram(wav)
expect_is(pulses <- find_pulses(spectrogram), "data.frame")
expect_identical(
  colnames(pulses),
  c("Pulse", "Xmin", "Xmax", "Ymin", "Ymax", "Ratio", "AmplitudeMin", "AmplitudeMax")
)
