context("wav2rds")
target.path <- tempdir()
source.path <- system.file("demo_wav", package = "rhinolophus")
to.copy <- list.files(source.path, pattern = "\\.wav$", full.names = TRUE)
file.copy(from = to.copy, to = target.path)
expect_warning(
  wav2rds(target.path, channel = "right"),
  ".*contains no information in the right channel"
)
wav2rds(target.path, te.factor = 10, channel = "left")
expect_identical(
  gsub("\\.wav$", "", list.files(target.path, pattern = "\\.wav$")),
  gsub("\\.rds$", "", list.files(target.path, pattern = "\\.rds$"))
)
file.remove(
  list.files(target.path, pattern = "\\.(wav|rds)$", full.names = TRUE)
)
