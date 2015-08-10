#' read a WAV file
#' @param filename The name of the file
#' @param channel Select the left or the right channel
#' @export
#' @importFrom tuneR readWave
read.wav <- function(filename, channel = c("left", "right")){
  channel <- match.arg(channel)
  header <- readWave(filename, header = TRUE)
  raw.data <- readWave(filename)
  if(channel == "left"){
    selected.channel <- raw.data@left
  } else {
    selected.channel <- raw.data@right
  }
  return(selected.channel)
}
