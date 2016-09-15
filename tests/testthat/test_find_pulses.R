context("find_pulses")
wav <- read_wav(
  system.file("demo_wav/leislers.wav", package = "rhinolophus")
)
spectrogram <- wav2spectrogram(wav)
expect_is(pulses <- find_pulses(spectrogram), "batPulse")
expect_error(
  find_pulses(spectrogram, min.amplitude = "a"),
  "min\\.amplitude is not a number \\(a length one numeric vector\\)\\."
)
expect_error(
  find_pulses(spectrogram, min.peak = "a"),
  "min\\.peak is not a number \\(a length one numeric vector\\)\\."
)
expect_error(
  find_pulses(spectrogram, min.amplitude = 10, min.peak = 0),
"min\\.peak not greater than or equal to min\\.amplitude \\+ min\\(delta\\.amplitude\\)"
)
expect_error(
  find_pulses("a"),
  "spectrogram does not inherit from class batSpectrogram"
)
