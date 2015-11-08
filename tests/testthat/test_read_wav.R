context("read_wav")
dir <- tempdir()
expect_error(
  read_wav(filename = dir),
  paste(normalizePath(dir, winslash = "/"), "is not a file")
)
expect_is(
  leislers <- read_wav(
    filename = system.file("demo_wav/leislers.wav", package = "rhinolophus")
  ),
  "batWav"
)
expect_identical(leislers@Metadata$SamplingRate, 441000)
expect_identical(leislers@Metadata$TimeExpansionFactor, 10L)
expect_identical(leislers@Metadata$Channel, factor("left", c("left", "right")))

expect_is(
  leislers.right <- read_wav(
    filename = system.file("demo_wav/leislers.wav", package = "rhinolophus"),
    channel = "right"
  ),
  "batWav"
)
expect_identical(length(leislers.right@Wav[[1]]), 0L)
