#' read a WAV file
#' @param filename The name of the file
#' @param channel Select the left or the right channel
#' @param te.factor The factor to which the original sound was slowed down prior
#'    to recording
#' @export
#' @importFrom assertthat assert_that is.string is.count
#' @importFrom tuneR readWave
#' @examples
#'  wav = read_wav(
#'    system.file("demo_wav/leislers.wav", package = "rhinolophus")
#'  )
read_wav <- function(
  filename,
  channel = c("left", "right"),
  te.factor = 10
){
  channel <- match.arg(channel)
  assert_that(is.string(filename))
  assert_that(is.count(te.factor))
  if (!file_test("-f", filename)) {
    stop(normalizePath(filename, winslash = "/"), " is not a file")
  }

  header <- readWave(filename, header = TRUE)
  raw.data <- readWave(filename)
  if (channel == "left") {
    selected.channel <- list(as.integer(raw.data@left))
  } else {
    selected.channel <- list(as.integer(raw.data@right))
  }
  names(selected.channel) <- sha1(
    list(
      selected.channel[[1]],
      as.integer(te.factor),
      as.numeric(header$sample.rate * te.factor)
    )
  )
  new(
    "batWav",
    Wav = selected.channel,
    Metadata = data.frame(
      Fingerprint = names(selected.channel),
      Filename = filename,
      Channel = factor(channel, levels = c("left", "right")),
      TimeExpansionFactor = as.integer(te.factor),
      SamplingRate = as.numeric(header$sample.rate * te.factor),
      stringsAsFactors = FALSE
    )
  )
}
