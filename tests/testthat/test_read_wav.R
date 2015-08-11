context("read_wav")
dir <- tempdir()
expect_error(
  read_wav(filename = dir),
  paste(dir, "does not exist")
)
file.copy(
  from = system.file("demo_wav/leislers.wav", package = "rhinolophus"),
  to = dir
)
leislers.file <- paste(dir, "leislers.wav", sep = "/")
expect_is(
  leislers <- read_wav(filename = leislers.file),
  "list"
)
expect_identical(leislers$sample.rate, 441000)
expect_identical(leislers$sample, 5065L)
expect_identical(leislers$sample, length(leislers$values))
