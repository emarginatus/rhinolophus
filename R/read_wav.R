#' read a WAV file
#' @param filename The name of the file
#' @param channel Select the left or the right channel
#' @param te.factor The factor to which the original sound was slowed down prior
#'    to recording
#' @export
#' @importFrom tuneR readWave
read_wav <- function(
  filename,
  channel = c("left", "right"),
  te.factor = 10
){
  channel <- match.arg(channel)
  header <- readWave(filename, header = TRUE)
  raw.data <- readWave(filename)
  if (channel == "left") {
    selected.channel <- raw.data@left
  } else {
    selected.channel <- raw.data@right
  }
  return(
    list(
      sample.rate = header$sample.rate * te.factor,
      sample = header$samples,
      values = selected.channel
    )
  )
}
