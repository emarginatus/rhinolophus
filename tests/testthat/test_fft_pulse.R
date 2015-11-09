context("fft_pulse")
wav <- read_wav(
  system.file("demo_wav/leislers.wav", package = "rhinolophus")
)
spectrogram <- wav2spectrogram(wav)
pulses <- find_pulses(spectrogram = spectrogram)
n.fourier <- 20L
pulse.fft <- fft_pulse(
  pulses = pulses,
  spectrogram = spectrogram,
  n.fourier = n.fourier
)

expect_is(pulse.fft, "batPulse")
expect_identical(
  names(pulse.fft@PulseFourier),
  pulse.fft@PulseMetadata$Fingerprint
)
